import backend/error
import gleam/dynamic
import gleam/hexpm
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/result
import gleam/uri

pub fn get_package_owners(package_name: String, secret hex_api_key: String) {
  use response <- result.try(
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path("/api/packages/" <> package_name <> "/owners")
    |> request.prepend_header("authorization", hex_api_key)
    |> request.prepend_header("user-agent", "gling / 0.0.0")
    |> httpc.send()
    |> result.map_error(error.FetchError),
  )

  response.body
  |> json.decode(using: dynamic.list(decode_hex_owner))
  |> result.map_error(error.JsonError)
}

pub fn get_package(package_name: String, secret hex_api_key: String) {
  use response <- result.try(
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path("/api/packages/" <> package_name)
    |> request.prepend_header("authorization", hex_api_key)
    |> request.prepend_header("user-agent", "gling / 0.0.0")
    |> httpc.send()
    |> result.map_error(error.FetchError),
  )

  response.body
  |> json.decode(using: hexpm.decode_package)
  |> result.map_error(error.JsonError)
}

fn decode_hex_owner(data) {
  dynamic.decode3(
    hexpm.PackageOwner,
    dynamic.field("username", dynamic.string),
    dynamic.optional_field("email", dynamic.string),
    dynamic.field("url", dynamic.string),
  )(data)
}

pub fn lookup_release(release: hexpm.PackageRelease, secret hex_api_key: String) {
  let assert Ok(url) = uri.parse(release.url)

  use response <- result.try(
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path(url.path)
    |> request.prepend_header("authorization", hex_api_key)
    |> request.prepend_header("user-agent", "gling / 0.0.0")
    |> httpc.send()
    |> result.map_error(error.FetchError),
  )

  response.body
  |> json.decode(using: hexpm.decode_release)
  |> result.map_error(error.JsonError)
}

pub fn get_api_packages_page(page: Int, hex_api_key: String) {
  use response <- result.try(
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path("/api/packages")
    |> request.set_query([
      #("sort", "updated_at"),
      #("page", int.to_string(page)),
    ])
    |> request.prepend_header("authorization", hex_api_key)
    |> request.prepend_header("user-agent", "gling / 0.0.0")
    |> httpc.send()
    |> result.map_error(error.FetchError),
  )

  response.body
  |> json.decode(using: dynamic.list(of: hexpm.decode_package))
  |> result.map_error(error.JsonError)
}
