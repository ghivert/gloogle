import cors_builder as cors
import gleam/bit_array
import gleam/bool
import gleam/crypto
import gleam/http
import gleam/http/request
import jupiter/context.{type Context}
import jupiter/context/environment/variables
import jupiter/router/handlers
import jupiter/web
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

/// Admin routes. Without `authorization: Bearer <ADMIN_TOKEN>` they answer
/// 404, as if they did not exist: each one makes jupiter call Hex with its
/// key and re-ingest whole packages, an open door to exhausting the instance
/// and the key's rate limit.
pub fn handle_post(req: Request, ctx: Context) {
  let header = request.get_header(req, "authorization")
  let refused = !authorized(header, variables.admin_token())
  use <- bool.guard(when: refused, return: wisp.not_found())
  case wisp.path_segments(req) {
    ["packages", "update", name] -> handlers.package_update(req, ctx, name)
    _ -> wisp.not_found()
  }
}

/// Whether an `authorization` header carries the admin token, compared in
/// constant time. No token configured: nobody is authorized.
pub fn authorized(
  header: Result(String, Nil),
  token: Result(String, Nil),
) -> Bool {
  case header, token {
    Ok("Bearer " <> given), Ok(token) ->
      crypto.secure_compare(
        bit_array.from_string(given),
        bit_array.from_string(token),
      )
    _, _ -> False
  }
}

pub fn handle_request(req: Request, ctx: Context) -> Response {
  use req <- cors.wisp_middleware(req, web.cors())
  use req <- web.foundations(req)
  use ctx <- context.set_trace_id(req, ctx)
  case req.method {
    http.Get -> handle_get(req, ctx)
    http.Post -> handle_post(req, ctx)
    _ -> wisp.not_found()
  }
}
