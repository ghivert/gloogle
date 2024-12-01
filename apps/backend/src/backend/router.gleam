import api/hex
import backend/context.{type Context}
import backend/error
import backend/gleam/type_search/msg
import backend/postgres/queries
import backend/web
import cors_builder as cors
import data/analytics
import data/package
import data/type_search
import gleam/erlang/process
import gleam/http
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string_tree
import tasks/hex as syncing
import wisp.{type Request, type Response}

fn empty_json() {
  let content = "{}"
  content
  |> string_tree.from_string()
  |> wisp.json_response(200)
}

fn search(query: String, ctx: Context) {
  wisp.log_notice("Searching for " <> query)
  let _ = queries.upsert_search_analytics(ctx.db, query)

  let exact_type_searches =
    option.then(ctx.type_search_subject, fn(subject) {
      process.try_call(subject, msg.Find(_, query), within: 25_000)
      |> option.from_result
      |> option.flatten
    })
    |> option.unwrap([])
    |> queries.exact_type_search(ctx.db, _)
    |> result.map_error(error.debug_log)
    |> result.unwrap([])

  let exact_name_matches =
    queries.name_search(ctx.db, query)
    |> result.map_error(error.debug_log)
    |> result.unwrap([])
    |> list.filter(fn(i) { !list.contains(exact_type_searches, i) })

  let exact_module_and_name_matches =
    queries.module_and_name_search(ctx.db, query)
    |> result.map_error(error.debug_log)
    |> result.unwrap([])
    |> list.filter(fn(i) {
      !list.contains(list.flatten([exact_type_searches, exact_name_matches]), i)
    })

  let exact_matches =
    list.flatten([exact_name_matches, exact_module_and_name_matches])

  let matches =
    queries.content_search(ctx.db, query)
    |> result.map_error(error.debug_log)
    |> result.unwrap([])
    |> list.filter(fn(i) {
      !list.contains(list.flatten([exact_matches, exact_type_searches]), i)
    })

  let signature_searches =
    queries.signature_search(ctx.db, query)
    |> result.map_error(error.debug_log)
    |> result.unwrap([])
    |> list.filter(fn(i) {
      !list.contains(
        list.flatten([exact_matches, exact_type_searches, matches]),
        i,
      )
    })

  let documentation_searches =
    queries.documentation_search(ctx.db, query)
    |> result.map_error(error.debug_log)
    |> result.unwrap([])
    |> list.filter(fn(i) {
      !list.contains(
        list.flatten([
          exact_matches,
          exact_type_searches,
          matches,
          signature_searches,
        ]),
        i,
      )
    })

  let module_searches =
    queries.module_search(ctx.db, query)
    |> result.map_error(error.debug_log)
    |> result.unwrap([])
    |> list.filter(fn(i) {
      !list.contains(
        list.flatten([
          exact_matches,
          exact_type_searches,
          matches,
          signature_searches,
          documentation_searches,
        ]),
        i,
      )
    })

  json.object([
    #("exact-type-matches", json.array(exact_type_searches, type_search.encode)),
    #("exact-matches", json.array(exact_matches, type_search.encode)),
    #("matches", json.array(matches, type_search.encode)),
    #("searches", json.array(signature_searches, type_search.encode)),
    #("docs-searches", {
      json.array(documentation_searches, type_search.encode)
    }),
    #("module-searches", { json.array(module_searches, type_search.encode) }),
  ])
}

pub fn handle_get(req: Request, ctx: Context) {
  case wisp.path_segments(req) {
    ["healthcheck"] -> wisp.ok()
    ["packages"] ->
      queries.select_package_by_updated_at(ctx.db)
      |> result.unwrap([])
      |> json.array(package.encode)
      |> json.to_string_tree
      |> wisp.json_response(200)
    ["trendings"] ->
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
    ["analytics"] -> {
      {
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
      |> result.map(fn(content) {
        content
        |> analytics.encode
        |> json.to_string_tree
        |> wisp.json_response(200)
      })
      |> result.map_error(error.debug_log)
      |> result.unwrap(wisp.internal_server_error())
    }
    ["search"] -> {
      wisp.get_query(req)
      |> list.find(fn(item) { item.0 == "q" })
      |> result.replace_error(error.EmptyError)
      |> result.map(fn(item) { search(item.1, ctx) })
      |> result.unwrap(json.object([#("error", json.string("internal"))]))
      |> json.to_string_tree
      |> wisp.json_response(200)
    }
    _ -> wisp.not_found()
  }
}

pub fn handle_post(req: Request, ctx: Context) {
  case wisp.path_segments(req) {
    ["packages", "update", name] -> {
      let _ =
        hex.get_package(name, ctx.hex_api_key)
        |> result.try(fn(package) { syncing.sync_package(ctx, package) })
      empty_json()
    }
    _ -> wisp.not_found()
  }
}

pub fn handle_request(req: Request, ctx: Context) -> Response {
  use req <- cors.wisp_middleware(req, web.cors())
  use req <- web.foundations(req)
  case req.method {
    http.Get -> handle_get(req, ctx)
    http.Post -> handle_post(req, ctx)
    _ -> wisp.not_found()
  }
}
