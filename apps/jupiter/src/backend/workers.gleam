import gleam/function
import gleam/otp/supervisor
import processes/periodic
import tasks/hex
import tasks/popularity
import tasks/ranking
import tasks/timeseries

pub fn sync_new_gleam_releases_ten_secondly(ctx, children) {
  use <- add_periodic_worker(children, waiting: 10_000)
  hex.sync_new_gleam_releases(ctx)
}

pub fn compute_ranking_daily(ctx, children) {
  use <- add_periodic_worker(children, waiting: 86_400_000)
  ranking.compute_ranking(ctx)
}

pub fn compute_popularity_daily(ctx, children) {
  use <- add_periodic_worker(children, waiting: 86_400_000)
  popularity.compute_popularity(ctx)
}

pub fn store_timeseries_hourly(ctx, children) {
  use <- add_periodic_worker(children, waiting: 3_600_000)
  timeseries.store_timeseries(ctx)
}

fn add_periodic_worker(children, waiting delay, do work) {
  use _ <- function.tap(children)
  supervisor.add(children, {
    use _ <- supervisor.worker()
    periodic.periodically(do: work, waiting: delay)
  })
}
