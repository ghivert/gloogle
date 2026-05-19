import gleam/erlang/process
import gleam/http
import gleam/otp/static_supervisor as supervisor
import gleam/result
import jupiter/context.{type Context}
import jupiter/context/environment/variables
import jupiter/context/postgres
import jupiter/gleam/type_search
import jupiter/router
import jupiter/workers
import mist
import palabres
import palabres/level
import palabres/options
import wisp/wisp_mist

pub fn main() {
  let assert Ok(_) = configure_logger()
  let assert Ok(ctx) = context.init()
  let assert Ok(postgres) = postgres.supervised()
  let assert Ok(_) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.auto_shutdown(supervisor.AnySignificant)
    |> supervisor.add(postgres)
    |> supervisor.add(http_server(ctx))
    |> supervisor.add(periodic_workers(ctx))
    |> supervisor.add(type_search.worker(ctx.db))
    |> supervisor.start
  process.sleep_forever()
}

fn configure_logger() {
  let log_level = variables.log_level()
  let log_level = level.from_string(log_level)
  use log_level <- result.map(log_level)
  options.defaults()
  |> options.level(log_level)
  |> options.color(True)
  |> palabres.configure
}

fn http_server(ctx: Context) {
  let secret_key_base = context.get_secret_key_base()
  router.handle_request(_, ctx)
  |> wisp_mist.handler(secret_key_base)
  |> mist.new
  |> mist.bind("0.0.0.0")
  |> mist.port(variables.port())
  |> mist.after_start(after_start)
  |> mist.supervised
}

fn after_start(port: Int, scheme: http.Scheme, _ip_address: mist.IpAddress) {
  palabres.info("Server started, listening")
  |> palabres.int("port", port)
  |> palabres.string("host", "0.0.0.0")
  |> palabres.string("scheme", http.scheme_to_string(scheme))
  |> palabres.log
}

fn periodic_workers(ctx: Context) {
  supervisor.new(supervisor.OneForOne)
  |> supervisor.add(workers.sync_new_gleam_releases_ten_secondly(ctx))
  |> supervisor.add(workers.compute_ranking_daily(ctx))
  |> supervisor.add(workers.compute_popularity_daily(ctx))
  |> supervisor.add(workers.store_timeseries_hourly(ctx))
  |> supervisor.supervised
}
