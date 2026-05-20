import gleam/function_
import gleam/list
import gleam/result
import jupiter/context.{type Context}
import jupiter/error
import jupiter/postgres/queries
import palabres

const module = "tasks/timeseries"

pub fn store_timeseries(ctx: Context) {
  palabres.info("Storing analytics timeseries")
  |> palabres.at(module:, function: "store_timeseries")
  |> palabres.log
  use analytics <- result.try(queries.select_last_day_search_analytics(ctx.db))
  use _ <- function_.tap(do_store_timeseries(ctx, analytics))
  palabres.info("Storing analytics finished!")
  |> palabres.at(module:, function: "store_timeseries")
  |> palabres.log
}

fn do_store_timeseries(ctx: Context, analytics: List(#(String, Int))) {
  analytics
  |> list.map(queries.upsert_search_analytics_timeseries(ctx.db, _))
  |> result.all
  |> result.map_error(error.debug_log)
}
