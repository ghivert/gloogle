import api/hex
import backend/config.{type Config, type Context}
import backend/error
import backend/postgres/postgres
import backend/web
import cors_builder as cors
import gleam/dynamic
import gleam/function
import gleam/http
import gleam/io
import gleam/json
import gleam/list
import gleam/pgo
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

pub fn handle_get(req: Request, ctx: Context) {
  case wisp.path_segments(req) {
    ["search"] -> {
      wisp.get_query(req)
      |> list.find(fn(item) { item.0 == "q" })
      |> result.replace_error(error.EmptyError)
      |> result.try(fn(item) {
        let query = pgo.text("'" <> item.1 <> "'")
        "SELECT
             s.name,
             s.json_signature,
             m.name,
             p.name
           FROM package_type_fun_signature s
           JOIN package_module m
           ON m.id = s.package_module_id
           JOIN package_release r
           ON r.id = m.package_release_id
           JOIN package p
           ON p.id = r.package_id
           WHERE to_tsvector(s.signature_) @@ to_tsquery($1)"
        |> pgo.execute(
          ctx.db,
          [query],
          dynamic.decode4(
            fn(a, b, c, d) {
              json.object([
                #("name", json.string(a)),
                #("json_signature", dynamic.unsafe_coerce(b)),
                #("module_name", json.string(c)),
                #("package_name", json.string(d)),
              ])
            },
            dynamic.element(0, dynamic.string),
            dynamic.element(1, dynamic.dynamic),
            dynamic.element(2, dynamic.string),
            dynamic.element(3, dynamic.string),
          ),
        )
        |> result.map_error(error.DatabaseError)
      })
      |> result.map(fn(r) { r.rows })
      |> result.unwrap([])
      |> function.tap(fn(a) { io.debug(list.length(a)) })
      |> json.preprocessed_array()
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
        |> io.debug()
      empty_json()
    }
    _ -> wisp.not_found()
  }
}

pub fn handle_request(req: Request, cnf: Config) -> Response {
  use req <- cors.wisp_handle(req, web.cors())
  use req <- web.foundations(req)
  use ctx <- postgres.middleware(cnf)
  case req.method {
    http.Get -> handle_get(req, ctx)
    http.Post -> handle_post(req, ctx)
    _ -> wisp.not_found()
  }
}
