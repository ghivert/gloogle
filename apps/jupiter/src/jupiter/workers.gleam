import gleam/otp/supervision
import gleam/time/duration
import processes/periodic
import tasks/hex
import tasks/popularity
import tasks/ranking
import tasks/timeseries

pub fn sync_new_gleam_releases_ten_secondly(ctx) {
  use <- add_periodic_worker(waiting: duration.seconds(10))
  hex.sync_new_gleam_releases(ctx)
}

pub fn compute_ranking_daily(ctx) {
  use <- add_periodic_worker(waiting: duration.seconds(86_400))
  ranking.compute_ranking(ctx)
}

pub fn compute_popularity_daily(ctx) {
  use <- add_periodic_worker(waiting: duration.seconds(86_400))
  popularity.compute_popularity(ctx)
}

pub fn store_timeseries_hourly(ctx) {
  use <- add_periodic_worker(waiting: duration.seconds(3600))
  timeseries.store_timeseries(ctx)
}

fn add_periodic_worker(waiting delay, do work) {
  use <- supervision.worker()
  periodic.periodically(do: work, waiting: delay)
}
