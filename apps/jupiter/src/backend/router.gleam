import backend/context.{type Context}
import backend/router/handlers
import backend/web
import cors_builder as cors
import gleam/http
import wisp.{type Request, type Response}

pub fn handle_get(req: Request, ctx: Context) {
  case wisp.path_segments(req) {
    ["healthcheck"] -> wisp.ok()
    ["packages"] -> handlers.packages(req, ctx)
    ["trendings"] -> handlers.trendings(req, ctx)
    ["analytics"] -> handlers.analytics(req, ctx)
    ["search"] -> handlers.search(req, ctx)
    _ -> wisp.not_found()
  }
}

pub fn handle_post(req: Request, ctx: Context) {
  case wisp.path_segments(req) {
    ["packages", "update", name] -> handlers.package_update(req, ctx, name)
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
