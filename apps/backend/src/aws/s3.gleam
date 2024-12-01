import aws4_request
import backend/context
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/option.{type Option, None, Some}
import gleam/result

fn request(url: String, method: http.Method, body: Option(BitArray)) {
  use bucket_uri <- result.try(context.bucket_uri())
  use #(access_key, secret_key) <- result.try(context.scaleway_keys())
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

fn sign(request, access_key_id, secret_access_key) {
  aws4_request.signer(
    access_key_id:,
    secret_access_key:,
    region: "fr-par",
    service: "s3",
  )
  |> aws4_request.sign_bits(request)
}

pub fn get(name: String) {
  use res <- result.try(request("/" <> name, http.Get, None))
  case res.status {
    200 -> Ok(res.body)
    _ -> Error(Nil)
  }
}

pub fn put(name: String, content: BitArray) {
  use res <- result.try(request("/" <> name, http.Put, Some(content)))
  case res.status {
    200 -> Ok(res.body)
    _ -> Error(Nil)
  }
}
