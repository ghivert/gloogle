import api/hex
import data/analytics
import data/ir
import data/ir/parse
import data/ir/unify
import data/package
import data/type_search
import gleam/bool
import gleam/function_
import gleam/int
import gleam/json
import gleam/list
import gleam/pair
import gleam/result
import jupiter/context.{type Context}
import jupiter/loss.{type Loss}
import jupiter/postgres/queries
import palabres
import pog
import tasks/hex as syncing
import wisp.{type Request}

const module = "jupiter/router/handlers"

pub fn analytics(_req: Request, ctx: Context) {
  select_all_analytics(ctx)
  |> result.map(fn(content) {
    content
    |> analytics.encode
    |> json.to_string
    |> wisp.json_response(200)
  })
  |> result.map_error(function_.tap(_, loss.log))
  |> result.unwrap(wisp.internal_server_error())
}

pub fn search(req: Request, ctx: Context) {
  wisp.get_query(req)
  |> list.find(fn(item) { item.0 == "q" })
  |> result.replace_error(loss.EmptyError)
  |> result.map(fn(item) { do_search(item.1, ctx) })
  |> result.unwrap(json.object([#("error", json.string("internal"))]))
  |> json.to_string
  |> wisp.json_response(200)
}

pub fn packages(_req: Request, ctx: Context) {
  queries.select_package_by_updated_at(ctx.db)
  |> result.unwrap([])
  |> json.array(package.encode)
  |> json.to_string
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
    |> json.to_string
    |> wisp.json_response(200)
  })
  |> result.unwrap(wisp.internal_server_error())
}

pub fn package_update(_req: Request, ctx: Context, name: String) {
  do_update_package(ctx, name)
  |> loss.dismiss
  wisp.json_response("{}", 200)
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
  palabres.notice("Search")
  |> palabres.string("query", query)
  |> palabres.at(module:, function: "do_search")
  |> palabres.log
  queries.upsert_search_analytics(ctx.db, query)
  |> loss.dismiss

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

/// The type-search tier: parse the query as a Gleam type, then run the two-phase
/// fingerprint + verify pipeline. `parse` fails on anything that is not a type
/// (multi-word text queries, malformed input) — those simply skip this tier and
/// fall through to the name/content tiers below.
fn exec_type_search(
  ctx: Context,
  query: String,
) -> List(type_search.TypeSearch) {
  case parse.parse(query) {
    Error(_) -> []
    Ok(type_) -> {
      let #(arity, type_names) = ir.fingerprint_type(type_)
      // A bare variable or hole (`a`, `_`) has no structural signal — arity 0 and
      // no names — so its fingerprint matches nearly everything. That is a text
      // query, not a type query; leave it to the other tiers.
      use <- bool.guard(
        when: arity == 0 && list.is_empty(type_names),
        return: [],
      )
      queries.fingerprint_candidates(ctx.db, arity, type_names)
      |> result.map_error(function_.tap(_, loss.log))
      |> result.unwrap([])
      |> list.filter_map(fn(candidate) {
        let #(item, declaration) = candidate
        unify.unify_declaration(type_, declaration)
        |> result.map(fn(match) { #(item, match) })
      })
      |> list.sort(fn(a, b) { unify.compare(a.1, b.1) })
      |> list.map(pair.first)
      |> list.take(20)
    }
  }
}

fn exec_search(
  ctx: Context,
  query: String,
  run: fn(pog.Connection, String) -> Loss(List(type_search.TypeSearch)),
  previous: List(type_search.TypeSearch),
) -> List(type_search.TypeSearch) {
  run(ctx.db, query)
  |> result.map_error(function_.tap(_, loss.log))
  |> result.unwrap([])
  |> list.filter(fn(i) { !list.contains(previous, i) })
}
