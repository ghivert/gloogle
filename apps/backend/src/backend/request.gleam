import gleam/http/request
import gleam/result
import wisp.{type Request}

pub fn is_json_request(req: Request) -> Bool {
  request.get_header(req, "accept")
  |> result.map(fn(content) { content == "application/json" })
  |> result.unwrap(False)
}
