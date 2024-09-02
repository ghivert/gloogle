import backend/config.{type Context}
import backend/error
import backend/postgres/queries
import gleam/function
import gleam/list
import gleam/result
import wisp

pub fn store_timeseries(ctx: Context) {
  wisp.log_info("Storing analytics timeseries")
  let query = queries.select_last_day_search_analytics(ctx.db)
  use analytics <- result.try(query)
  use _ <- function.tap({
    result.all({
      use analytic <- list.map(analytics)
      queries.upsert_search_analytics_timeseries(ctx.db, analytic)
    })
    |> result.map_error(error.debug_log)
  })
  wisp.log_info("Storing analytics finished!")
}
