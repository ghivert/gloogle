import backend/context
import cors_builder as cors_
import gleam/http
import palabre
import wisp.{type Request, type Response}

pub type Handler =
  fn(Request) -> Response

pub fn foundations(req: Request, handler: Handler) -> Response {
  use <- palabre.log_request(req)
  use <- wisp.rescue_crashes()
  use req <- wisp.handle_head(req)
  handler(req)
}

pub fn cors() {
  let origin = select_origin()
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

fn select_origin() {
  case context.read_environment() {
    context.Development -> cors_.allow_origin(_, "http://localhost:5173")
    context.Production -> allow_production
  }
}

fn allow_production(cors: cors_.Cors) -> cors_.Cors {
  cors
  |> cors_.allow_origin("https://gloogle.run")
  |> cors_.allow_origin("https://www.gloogle.run")
}
