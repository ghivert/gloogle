import backend/index/error
import gleam/dynamic
import gleam/hackney
import gleam/hexpm
import gleam/http/request
import gleam/io
import gleam/json
import gleam/result

pub fn get_package_owners(package_name: String, secret hex_api_key: String) {
  use response <- result.try(
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path("/api/packages/" <> package_name <> "/owners")
    |> request.prepend_header("authorization", hex_api_key)
    |> hackney.send()
    |> result.map_error(error.FetchError),
  )

  response.body
  |> io.debug()
  |> json.decode(using: dynamic.list(decode_hex_owner))
  |> result.map_error(error.JsonError)
}

fn decode_hex_owner(data) {
  dynamic.decode3(
    hexpm.PackageOwner,
    dynamic.field("username", dynamic.string),
    dynamic.optional(dynamic.field("email", dynamic.string)),
    dynamic.field("url", dynamic.string),
  )(data)
}
