import backend/config
import backend/request
import gleam/http
import wisp.{type Request, type Response}

pub type Handler =
  fn(Request) -> Response

pub fn foundations(req: Request, handler: Handler) -> Response {
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes()
  use req <- wisp.handle_head(req)
  handler(req)
}

pub fn reroute_non_json_request(req: Request, handler: Handler) -> Response {
  case req.method, request.is_json_request(req), config.is_dev() {
    http.Get, True, _ -> handler(req)
    http.Get, False, True -> wisp.redirect("http://localhost:5173")
    http.Get, False, False -> wisp.ok()
    _, _, _ -> handler(req)
  }
}
