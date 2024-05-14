import backend/config
import cors_builder as cors
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

pub fn cors() {
  let origin = case config.is_dev() {
    True -> "http://localhost:5173"
    False -> "https://gloogle.run"
  }
  cors.new()
  |> cors.allow_origin(origin)
  |> cors.allow_method(http.Get)
  |> cors.allow_method(http.Post)
  |> cors.allow_method(http.Put)
  |> cors.allow_method(http.Patch)
  |> cors.max_age(86_400)
}
