import backend/config.{type Config}
import backend/index/connect as postgres
import backend/web.{type Handler}
import gleam/string_builder
import wisp.{type Request, type Response}

fn empty_json() {
  let content = "{}"
  content
  |> string_builder.from_string()
  |> wisp.json_response(200)
}

pub fn handle_request(req: Request, cnf: Config) -> Response {
  use req <- web.foundations(req)
  use req <- web.reroute_non_json_request(req)
  use ctx <- postgres.middleware(cnf)
  case wisp.path_segments(req) {
    [] -> empty_json()
    _ -> wisp.not_found()
  }
}
