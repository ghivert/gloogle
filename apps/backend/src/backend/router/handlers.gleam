import api/hex
import backend/context.{type Context}
import backend/error
import backend/gleam/type_search/msg
import backend/postgres/queries
import data/analytics
import data/package
import data/type_search
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string_tree
import pog
import tasks/hex as syncing
import wisp.{type Request}

pub fn analytics(_req: Request, ctx: Context) {
  select_all_analytics(ctx)
  |> result.map(fn(content) {
    content
    |> analytics.encode
    |> json.to_string_tree
    |> wisp.json_response(200)
  })
  |> result.map_error(error.debug_log)
  |> result.unwrap(wisp.internal_server_error())
}

pub fn search(req: Request, ctx: Context) {
  wisp.get_query(req)
  |> list.find(fn(item) { item.0 == "q" })
  |> result.replace_error(error.EmptyError)
  |> result.map(fn(item) { do_search(item.1, ctx) })
  |> result.unwrap(json.object([#("error", json.string("internal"))]))
  |> json.to_string_tree
  |> wisp.json_response(200)
}

pub fn packages(_req: Request, ctx: Context) {
  queries.select_package_by_updated_at(ctx.db)
  |> result.unwrap([])
  |> json.array(package.encode)
  |> json.to_string_tree
  |> wisp.json_response(200)
}

pub fn trendings(req: Request, ctx: Context) {
  wisp.get_query(req)
  |> list.find(fn(item) { item.0 == "page" })
  |> result.try(fn(item) { int.parse(item.1) })
  |> result.try_recover(fn(_) { Ok(0) })
  |> result.unwrap(0)
  |> queries.select_package_by_popularity(ctx.db, _)
  |> result.map(fn(content) {
    content
    |> json.array(package.encode)
    |> json.to_string_tree
    |> wisp.json_response(200)
  })
  |> result.unwrap(wisp.internal_server_error())
}

pub fn package_update(_req: Request, ctx: Context, name: String) {
  let _ = do_update_package(ctx, name)
  "{}"
  |> string_tree.from_string
  |> wisp.json_response(200)
}

fn do_update_package(ctx: Context, name: String) {
  let found_package = hex.get_package(name, ctx.hex_api_key)
  use package <- result.try(found_package)
  syncing.sync_package(ctx, package)
}

fn select_all_analytics(ctx: Context) {
  use timeseries <- result.try(queries.get_timeseries_count(ctx.db))
  use total_searches <- result.try(queries.get_total_searches(ctx.db))
  use signatures <- result.try(queries.get_total_signatures(ctx.db))
  use packages <- result.try(queries.get_total_packages(ctx.db))
  use #(ranked, popular) <- result.try({
    queries.select_more_popular_packages(ctx.db)
  })
  let total_searches = list.first(total_searches) |> result.unwrap(0)
  let total_signatures = list.first(signatures) |> result.unwrap(0)
  let total_indexed = list.first(packages) |> result.unwrap(0)
  Ok(analytics.Analytics(
    timeseries:,
    total_searches:,
    total_signatures:,
    total_indexed:,
    ranked:,
    popular:,
  ))
}

fn do_search(query: String, ctx: Context) {
  wisp.log_notice("Searching for " <> query)
  let _ = queries.upsert_search_analytics(ctx.db, query)

  let exact_type_searches = exec_type_search(ctx, query)

  let exact_name_matches =
    exact_type_searches
    |> exec_search(ctx, query, queries.name_search, _)

  let exact_module_and_name_matches =
    [exact_type_searches, exact_name_matches]
    |> list.flatten
    |> exec_search(ctx, query, queries.module_and_name_search, _)

  let exact_matches =
    list.flatten([exact_name_matches, exact_module_and_name_matches])

  let matches =
    [exact_matches, exact_type_searches]
    |> list.flatten
    |> exec_search(ctx, query, queries.content_search, _)

  let signature_searches =
    [exact_matches, exact_type_searches, matches]
    |> list.flatten
    |> exec_search(ctx, query, queries.signature_search, _)

  let documentation_searches =
    [exact_matches, exact_type_searches, matches, signature_searches]
    |> list.flatten
    |> exec_search(ctx, query, queries.documentation_search, _)

  let module_searches =
    [
      exact_matches,
      exact_type_searches,
      matches,
      signature_searches,
      documentation_searches,
    ]
    |> list.flatten
    |> exec_search(ctx, query, queries.module_search, _)

  json.object([
    #("exact-type-matches", json.array(exact_type_searches, type_search.encode)),
    #("exact-matches", json.array(exact_matches, type_search.encode)),
    #("matches", json.array(matches, type_search.encode)),
    #("searches", json.array(signature_searches, type_search.encode)),
    #("docs-searches", json.array(documentation_searches, type_search.encode)),
    #("module-searches", { json.array(module_searches, type_search.encode) }),
  ])
}

fn exec_type_search(ctx: Context, query: String) {
  option.then(ctx.type_search_subject, fn(subject) {
    process.try_call(subject, msg.Find(_, query), within: 25_000)
    |> option.from_result
    |> option.flatten
  })
  |> option.unwrap([])
  |> queries.exact_type_search(ctx.db, _)
  |> result.map_error(error.debug_log)
  |> result.unwrap([])
}

fn exec_search(
  ctx: Context,
  query: String,
  run: fn(pog.Connection, String) ->
    Result(List(type_search.TypeSearch), error.Error),
  previous: List(type_search.TypeSearch),
) -> List(type_search.TypeSearch) {
  run(ctx.db, query)
  |> result.map_error(error.debug_log)
  |> result.unwrap([])
  |> list.filter(fn(i) { !list.contains(previous, i) })
}
