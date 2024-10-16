import api/hex
import backend/config.{type Context}
import backend/error
import backend/gleam/type_search/msg as type_search
import backend/postgres/queries
import backend/web
import birl
import cors_builder as cors
import gleam/erlang/process
import gleam/http
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/result
import gleam/string_builder
import tasks/hex as syncing
import wisp.{type Request, type Response}

fn empty_json() {
  let content = "{}"
  content
  |> string_builder.from_string()
  |> wisp.json_response(200)
}

fn search(query: String, ctx: Context) {
  wisp.log_notice("Searching for " <> query)
  let _ = queries.upsert_search_analytics(ctx.db, query)

  let exact_type_searches =
    option.then(ctx.type_search_subject, fn(subject) {
      process.try_call(subject, type_search.Find(_, query), within: 25_000)
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
      !list.contains(list.concat([exact_type_searches, exact_name_matches]), i)
    })

  let exact_matches =
    list.concat([exact_name_matches, exact_module_and_name_matches])

  let matches =
    queries.content_search(ctx.db, query)
    |> result.map_error(error.debug_log)
    |> result.unwrap([])
    |> list.filter(fn(i) {
      !list.contains(list.concat([exact_matches, exact_type_searches]), i)
    })

  let signature_searches =
    queries.signature_search(ctx.db, query)
    |> result.map_error(error.debug_log)
    |> result.unwrap([])
    |> list.filter(fn(i) {
      !list.contains(
        list.concat([exact_matches, exact_type_searches, matches]),
        i,
      )
    })

  let documentation_searches =
    queries.documentation_search(ctx.db, query)
    |> result.map_error(error.debug_log)
    |> result.unwrap([])
    |> list.filter(fn(i) {
      !list.contains(
        list.concat([
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
        list.concat([
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
    #(
      "exact-type-matches",
      json.array(exact_type_searches, queries.type_search_to_json),
    ),
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

fn encode_package(package: #(String, String, Int, option.Option(Int))) {
  let #(name, repository, rank, popularity) = package
  json.object([
    #("name", json.string(name)),
    #("repository", json.string(repository)),
    #("rank", json.int(rank)),
    #("popularity", json.nullable(popularity, json.int)),
  ])
}

pub fn handle_get(req: Request, ctx: Context) {
  case wisp.path_segments(req) {
    ["healthcheck"] -> wisp.ok()
    ["packages"] ->
      queries.select_package_by_updated_at(ctx.db)
      |> result.unwrap([])
      |> json.preprocessed_array
      |> json.to_string_builder
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
        |> json.preprocessed_array()
        |> json.to_string_builder()
        |> wisp.json_response(200)
      })
      |> result.unwrap(wisp.internal_server_error())
    ["analytics"] -> {
      {
        use timeseries <- result.try(queries.get_timeseries_count(ctx.db))
        use total <- result.try(queries.get_total_searches(ctx.db))
        use signatures <- result.try(queries.get_total_signatures(ctx.db))
        use packages <- result.try(queries.get_total_packages(ctx.db))
        use #(ranked, popular) <- result.try({
          queries.select_more_popular_packages(ctx.db)
        })
        let total = list.first(total) |> result.unwrap(0)
        let signatures = list.first(signatures) |> result.unwrap(0)
        let packages = list.first(packages) |> result.unwrap(0)
        Ok(#(timeseries, total, signatures, packages, ranked, popular))
      }
      |> result.map(fn(content) {
        let #(timeseries, total, signatures, packages, ranked, popular) =
          content
        json.object([
          #("total", json.int(total)),
          #("signatures", json.int(signatures)),
          #("packages", json.int(packages)),
          #("ranked", json.array(ranked, encode_package)),
          #("popular", json.array(popular, encode_package)),
          #("timeseries", {
            json.array(timeseries, fn(row) {
              json.object([
                #("count", json.int(row.0)),
                #("date", json.string(birl.to_iso8601(row.1))),
              ])
            })
          }),
        ])
        |> json.to_string_builder
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
