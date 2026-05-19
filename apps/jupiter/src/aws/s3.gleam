import aws4_request
import gleam/http
import gleam/http/request.{type Request}
import gleam/httpc
import gleam/option.{type Option, None, Some}
import gleam/result
import jupiter/context/environment/variables

fn request(url: String, method: http.Method, body: Option(BitArray)) {
  let bucket_uri = variables.bucket_uri()
  let #(access_key, secret_key) = variables.scaleway_keys()
  request.new()
  |> request.set_method(method)
  |> request.set_path(url)
  |> request.set_body(option.unwrap(body, <<>>))
  |> request.set_host(bucket_uri)
  |> request.set_scheme(http.Https)
  |> request.set_header("content-type", "application/octet-stream")
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
