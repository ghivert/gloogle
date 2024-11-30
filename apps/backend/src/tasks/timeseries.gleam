import backend/context.{type Context}
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
  analytics
  |> list.map(queries.upsert_search_analytics_timeseries(ctx.db, _))
  |> result.all
  |> result.map_error(error.debug_log)
  |> function.tap(fn(_) { wisp.log_info("Storing analytics finished!") })
}
