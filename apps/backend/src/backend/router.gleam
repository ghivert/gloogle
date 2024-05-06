import api/hex
import backend/config.{type Config, type Context}
import backend/postgres/postgres
import backend/web
import gleam/http
import gleam/io
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
    [] -> empty_json()
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
  use req <- web.foundations(req)
  use req <- web.reroute_non_json_request(req)
  use ctx <- postgres.middleware(cnf)
  case req.method {
    http.Get -> handle_get(req, ctx)
    http.Post -> handle_post(req, ctx)
    _ -> wisp.not_found()
  }
}
