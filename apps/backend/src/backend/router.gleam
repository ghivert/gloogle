import api/hex
import backend/config.{type Context}
import backend/error
import backend/postgres/queries
import backend/web
import cors_builder as cors
import gleam/bool
import gleam/http
import gleam/json
import gleam/list
import gleam/pair
import gleam/result
import gleam/string
import gleam/string_builder
import tasks/hex as syncing
import wisp.{type Request, type Response}

fn empty_json() {
  let content = "{}"
  content
  |> string_builder.from_string()
  |> wisp.json_response(200)
}

fn isolate_filters(query: String) -> #(String, List(String)) {
  string.split(query, " ")
  |> list.fold(#([], []), fn(acc, val) {
    case val {
      "in:signature" | "in:name" | "in:documentation" | "in:module" -> #(acc.0, [
        val,
        ..acc.1
      ])
      _ -> #([val, ..acc.0], acc.1)
    }
  })
  |> pair.map_first(list.reverse)
  |> pair.map_first(string.join(_, " "))
  |> pair.map_second(fn(filters) {
    let no_filters = list.is_empty(filters)
    use <- bool.guard(when: no_filters, return: [
      "in:signature", "in:name", "in:documentation", "in:module",
    ])
    filters
  })
}

fn search(query: String, ctx: Context) {
  wisp.log_notice("Searching for " <> query)
  let #(query, filters) = isolate_filters(query)
  let exact_matches = case list.contains(filters, "in:name") {
    False -> []
    True ->
      queries.name_search(ctx.db, query)
      |> result.map_error(error.debug_log)
      |> result.unwrap([])
  }
  let matches = case list.contains(filters, "in:signature") {
    False -> []
    True ->
      queries.content_search(ctx.db, query)
      |> result.map_error(error.debug_log)
      |> result.unwrap([])
      |> list.filter(fn(i) { !list.contains(exact_matches, i) })
  }
  let signature_searches = case list.contains(filters, "in:signature") {
    False -> []
    True ->
      queries.signature_search(ctx.db, query)
      |> result.map_error(error.debug_log)
      |> result.unwrap([])
      |> list.filter(fn(i) {
        !list.contains(list.append(exact_matches, matches), i)
      })
  }
  let documentation_searches = case list.contains(filters, "in:documentation") {
    False -> []
    True ->
      queries.documentation_search(ctx.db, query)
      |> result.map_error(error.debug_log)
      |> result.unwrap([])
      |> list.filter(fn(i) {
        !list.contains(list.append(exact_matches, matches), i)
      })
  }
  let module_searches = case list.contains(filters, "in:module") {
    False -> []
    True ->
      queries.module_search(ctx.db, query)
      |> result.map_error(error.debug_log)
      |> result.unwrap([])
      |> list.filter(fn(i) {
        !list.contains(list.append(exact_matches, matches), i)
      })
  }
  json.object([
    #("exact-matches", json.array(exact_matches, queries.type_search_to_json)),
    #("matches", json.array(matches, queries.type_search_to_json)),
    #("searches", json.array(signature_searches, queries.type_search_to_json)),
    #("docs-searches", {
      json.array(documentation_searches, queries.type_search_to_json)
    }),
    #("module-searches", {
      json.array(module_searches, queries.type_search_to_json)
    }),
  ])
}

pub fn handle_get(req: Request, ctx: Context) {
  case wisp.path_segments(req) {
    ["healthcheck"] -> wisp.ok()
    ["search"] -> {
      wisp.get_query(req)
      |> list.find(fn(item) { item.0 == "q" })
      |> result.replace_error(error.EmptyError)
      |> result.map(fn(item) { search(item.1, ctx) })
      |> result.unwrap(json.object([#("error", json.string("internal"))]))
      |> json.to_string_builder()
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
