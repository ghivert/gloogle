import aws4_request
import gleam/http
import gleam/http/request.{type Request}
import gleam/httpc
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/uri
import jupiter/context/environment/variables

/// Where the archive bucket lives. `BUCKET_URI` takes two shapes:
///
/// - a bare virtual-hosted host, the production form —
///   `hex-mirror.s3.fr-par.scw.cloud` — reached over https, objects at `/<key>`;
/// - a full URL with a bucket path, the local-stack form —
///   `http://localhost:9000/gloogle` — scheme and port as written, objects at
///   `/<bucket>/<key>` (path-style, which is what RustFS speaks).
type Endpoint {
  Endpoint(scheme: http.Scheme, host: String, port: Option(Int), prefix: String)
}

fn endpoint() -> Endpoint {
  let bucket_uri = variables.bucket_uri()
  case uri.parse(bucket_uri) {
    Ok(uri.Uri(scheme: Some(scheme), host: Some(host), port:, path:, ..)) -> {
      let scheme = case scheme {
        "http" -> http.Http
        _ -> http.Https
      }
      let prefix = case string.ends_with(path, "/") {
        True -> string.drop_end(path, 1)
        False -> path
      }
      Endpoint(scheme:, host:, port:, prefix:)
    }
    _ -> Endpoint(scheme: http.Https, host: bucket_uri, port: None, prefix: "")
  }
}

fn request(url: String, method: http.Method, body: Option(BitArray)) {
  let Endpoint(scheme:, host:, port:, prefix:) = endpoint()
  let #(access_key, secret_key) = variables.scaleway_keys()
  let request =
    request.new()
    |> request.set_method(method)
    |> request.set_path(prefix <> url)
    |> request.set_body(option.unwrap(body, <<>>))
    |> request.set_host(host)
    |> request.set_scheme(scheme)
    |> request.set_header("content-type", "application/octet-stream")
  let request = case port {
    Some(port) -> request.set_port(request, port)
    None -> request
  }
  request
  |> sign(access_key, secret_key)
  |> httpc.send_bits()
  |> result.replace_error(Nil)
}

fn sign(
  request: Request(BitArray),
  access_key_id: String,
  secret_access_key: String,
) -> Request(BitArray) {
  let region = "fr-par"
  let service = "s3"
  aws4_request.signer(access_key_id:, secret_access_key:, region:, service:)
  |> aws4_request.sign_bits(request)
}

pub fn get(name: String) -> Result(BitArray, Nil) {
  use res <- result.try(request("/" <> name, http.Get, None))
  case res.status {
    200 -> Ok(res.body)
    _ -> Error(Nil)
  }
}

pub fn put(name: String, content: BitArray) -> Result(BitArray, Nil) {
  use res <- result.try(request("/" <> name, http.Put, Some(content)))
  case res.status {
    200 -> Ok(res.body)
    _ -> Error(Nil)
  }
}
