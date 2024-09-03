import backend/config
import cors_builder as cors_
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
    True -> cors_.allow_origin(_, "http://localhost:5173")
    False -> fn(cors) {
      cors
      |> cors_.allow_origin("https://gloogle.run")
      |> cors_.allow_origin("https://www.gloogle.run")
    }
  }
  cors_.new()
  |> origin()
  |> cors_.allow_method(http.Get)
  |> cors_.allow_method(http.Post)
  |> cors_.allow_method(http.Put)
  |> cors_.allow_method(http.Patch)
  |> cors_.allow_header("baggage")
  |> cors_.allow_header("sentry-trace")
  |> cors_.max_age(86_400)
}
