import gleam/bool
import gleam/dynamic.{type Decoder, type Dynamic}
import gleam/fetch
import gleam/http
import gleam/http/request
import gleam/http/response
import gleam/io
import gleam/javascript/promise
import gleam/list
import gleam/result
import gleam/string
import gleam/uri
import vitools

pub type DiscussError {
  InternalServerError
  InvalidJsonBody
  DecodeError(List(dynamic.DecodeError))
  NetworkError
  NotFound
}

pub opaque type Discuss(a) {
  Discuss(
    path: String,
    query: List(#(String, String)),
    method: http.Method,
    decoder: Decoder(a),
    on_success: fn(a) -> Nil,
    on_error: fn(DiscussError) -> Nil,
  )
}

pub fn about(segments: List(String)) {
  let path = encode_path(segments)
  Discuss(
    path:,
    query: [],
    method: http.Get,
    decoder: fn(a: Dynamic) { Ok(a) },
    on_success: fn(_) { Nil },
    on_error: fn(_) { Nil },
  )
}

pub fn via(discuss: Discuss(a), method method: http.Method) {
  Discuss(..discuss, method:)
}

pub fn query(discuss: Discuss(a), query: List(#(String, String))) {
  Discuss(..discuss, query:)
}

pub fn expect(discuss: Discuss(a), format decoder: Decoder(b)) {
  let Discuss(path:, query:, method:, ..) = discuss
  Discuss(
    path:,
    decoder:,
    method:,
    query:,
    on_success: fn(_) { Nil },
    on_error: fn(_) { Nil },
  )
}

pub fn on_success(discuss: Discuss(a), run on_success: fn(a) -> Nil) {
  Discuss(..discuss, on_success:)
}

pub fn on_error(discuss: Discuss(a), run on_error: fn(DiscussError) -> Nil) {
  Discuss(..discuss, on_error:)
}

pub fn start(discuss: Discuss(a)) {
  let assert Ok(req) = endpoint()
  req
  |> request.set_path(discuss.path)
  |> request.set_query(discuss.query)
  |> fetch.send()
  |> promise.try_await(fetch.read_json_body)
  |> promise.map(fn(result) { result.map_error(result, map_fetch_error) })
  |> promise.tap(fn(res) {
    case res {
      Error(error) -> discuss.on_error(error)
      Ok(res) -> {
        let warn = fn(error) { fn() { discuss.on_error(error) } }
        use <- handle_status(res, 404, warn(NotFound))
        use <- handle_status(res, 500, warn(InternalServerError))
        case discuss.decoder(res.body) {
          Error(error) -> discuss.on_error(DecodeError(error))
          Ok(res) -> discuss.on_success(res)
        }
      }
    }
  })
}

fn encode_path(segments: List(String)) {
  segments
  |> list.map(uri.percent_encode)
  |> string.join("/")
  |> prepend_slash
}

fn prepend_slash(path) {
  case path {
    "/" <> path -> path
    path -> "/" <> path
  }
}

fn endpoint() {
  case vitools.is_dev() {
    True -> "http://localhost:3000"
    False -> "https://api.gloogle.run"
  }
  |> request.to
}

fn map_fetch_error(error: fetch.FetchError) {
  case error {
    fetch.InvalidJsonBody -> InvalidJsonBody
    fetch.NetworkError(_) -> NetworkError
    fetch.UnableToReadBody -> InvalidJsonBody
  }
}

fn handle_status(
  response: response.Response(a),
  status_code: Int,
  return: fn() -> b,
  continuation: fn() -> b,
) {
  let is_matching_status = status_code == response.status
  use <- bool.lazy_guard(when: is_matching_status, return:)
  continuation()
}
