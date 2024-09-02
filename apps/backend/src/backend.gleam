import backend/config
import backend/gleam/type_search/state as type_search
import backend/postgres/postgres
import backend/router
import dot_env
import gleam/erlang/process
import gleam/function
import gleam/otp/supervisor
import mist
import periodic
import setup
import tasks/hex
import tasks/popularity
import tasks/ranking
import tasks/timeseries
import wisp
import wisp/logger

pub fn main() {
  wisp.configure_logger()
  dot_env.load()

  let secret_key_base = config.get_secret_key_base()
  let cnf = config.read_config()
  let ctx = postgres.connect(cnf)

  logger.set_level(cnf.level)
  setup.radiate()

  let assert Ok(subject) = type_search.init(ctx.db)

  let ctx = ctx |> config.add_type_search_subject(subject)

  let assert Ok(_) =
    router.handle_request(_, ctx)
    |> wisp.mist_handler(secret_key_base)
    |> mist.new()
    |> mist.port(cnf.port)
    |> mist.start_http()

  let assert Ok(_) = {
    use periodic_children <- supervisor.start()
    use periodic_children <- function.tap(periodic_children)
    let assert Ok(_) = {
      use children <- supervisor.start()
      // Every 10 seconds
      add_periodic_worker(periodic_children, waiting: 10 * 1000, do: fn() {
        hex.sync_new_gleam_releases(ctx, children)
      })
      // Every day
      add_periodic_worker(periodic_children, waiting: 86_400_000, do: fn() {
        ranking.compute_ranking(ctx)
      })
      // Every day
      add_periodic_worker(periodic_children, waiting: 86_400_000, do: fn() {
        popularity.compute_popularity(ctx)
      })
      // Every hour
      add_periodic_worker(periodic_children, waiting: 3600 * 1000, do: fn() {
        timeseries.store_timeseries(ctx)
      })
    }
  }

  process.sleep_forever()
}

fn add_periodic_worker(children, waiting delay, do work) {
  use _ <- function.tap(children)
  supervisor.add(children, {
    use _ <- supervisor.worker()
    periodic.periodically(do: work, waiting: delay)
  })
}
