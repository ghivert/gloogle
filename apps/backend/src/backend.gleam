import backend/context.{type Context, Context}
import backend/gleam/type_search/state as type_search
import backend/router
import backend/setup
import backend/workers
import envoy
import gleam/erlang/process
import gleam/function
import gleam/int
import gleam/option.{Some}
import gleam/otp/supervisor
import gleam/result
import mist
import wisp
import wisp/logger
import wisp/wisp_mist

pub fn main() {
  configure_logger()
  let assert Ok(ctx) = context.init()
  let assert Ok(ctx) = start_type_search_worker(ctx)
  let assert Ok(_) = start_http_server(ctx)
  let assert Ok(_) = start_periodic_workers(ctx)
  process.sleep_forever()
}

fn configure_logger() {
  let level = logger.read_level()
  wisp.configure_logger()
  logger.set_level(level)
  setup.radiate()
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
  |> mist.new()
  |> mist.port(port)
  |> mist.start_http()
}

fn start_periodic_workers(ctx) {
  use children <- supervisor.start()
  use children <- function.tap(children)
  workers.sync_new_gleam_releases_ten_secondly(ctx, children)
  workers.compute_ranking_daily(ctx, children)
  workers.compute_popularity_daily(ctx, children)
  workers.store_timeseries_hourly(ctx, children)
}
