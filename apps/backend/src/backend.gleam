import backend/context.{type Context, Context}
import backend/gleam/type_search/state as type_search
import backend/router
import backend/workers
import envoy
import gleam/erlang/process
import gleam/function
import gleam/int
import gleam/option.{Some}
import gleam/otp/supervisor
import gleam/result
import mist
import palabre
import palabre/level
import wisp/wisp_mist

pub fn main() {
  let assert Ok(_) = configure_logger()
  let assert Ok(ctx) = context.init()
  let assert Ok(ctx) = start_type_search_worker(ctx)
  let assert Ok(_) = start_http_server(ctx)
  let assert Ok(_) = start_periodic_workers(ctx)
  process.sleep_forever()
}

fn configure_logger() {
  let log_level = envoy.get("LOG_LEVEL") |> result.unwrap("INFO")
  use log_level <- result.map(level.from_string(log_level))
  palabre.options()
  |> palabre.json(False)
  |> palabre.level(log_level)
  |> palabre.color(True)
  |> palabre.configure
}

fn start_type_search_worker(ctx: Context) {
  use subject <- result.map(type_search.init(ctx.db))
  Context(..ctx, type_search_subject: Some(subject))
}

fn start_http_server(ctx) {
  use port <- result.try(envoy.get("PORT"))
  use port <- result.map(int.parse(port))
  let secret_key_base = context.get_secret_key_base()
  router.handle_request(_, ctx)
  |> wisp_mist.handler(secret_key_base)
  |> mist.new
  |> mist.bind("0.0.0.0")
  |> mist.port(port)
  |> mist.start_http_server
}

fn start_periodic_workers(ctx) {
  use children <- supervisor.start()
  use children <- function.tap(children)
  workers.sync_new_gleam_releases_ten_secondly(ctx, children)
  workers.compute_ranking_daily(ctx, children)
  workers.compute_popularity_daily(ctx, children)
  workers.store_timeseries_hourly(ctx, children)
}
