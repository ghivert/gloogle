import aws4_request
import backend/config
import birl
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/option.{type Option, None, Some}
import gleam/result

fn request(url: String, method: http.Method, body: Option(BitArray)) {
  let date = birl.to_erlang_universal_datetime(birl.now())
  use bucket_uri <- result.try(config.bucket_uri())
  use #(access_key, secret_key) <- result.try(config.scaleway_keys())
  request.new()
  |> request.set_method(method)
  |> request.set_path(url)
  |> request.set_body(option.unwrap(body, <<>>))
  |> request.set_host(bucket_uri)
  |> request.set_scheme(http.Https)
  |> request.set_header("content-type", "application/octet-stream")
  |> aws4_request.sign(date, access_key, secret_key, "fr-par", "s3")
  |> httpc.send_bits()
  |> result.nil_error()
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
