import gleam/string_builder
import wisp.{type Request, type Response}
import backend/web

fn empty_json() {
  let content = "{}"
  content
  |> string_builder.from_string()
  |> wisp.json_response(200)
}

pub fn handle_request(req: Request) -> Response {
  use req <- web.middleware(req)
  use req <- web.reroute_non_json_request(req)
  case wisp.path_segments(req) {
    [] -> empty_json()
    _ -> wisp.not_found()
  }
}
