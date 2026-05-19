import decrypt
import gleam/dynamic/decode
import gleam/hexpm
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/result
import gleam/uri
import jupiter/error

pub fn get_package_owners(package_name: String, secret hex_api_key: String) {
  use response <- result.try({
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path("/api/packages/" <> package_name <> "/owners")
    |> request.prepend_header("authorization", hex_api_key)
    |> request.prepend_header("user-agent", "gloogle / 1.0.0")
    |> httpc.send()
    |> result.map_error(error.HttpcError)
  })

  response.body
  |> json.parse(using: decode.list(hex_owner_decoder()))
  |> result.map_error(error.JsonError)
}

pub fn get_package(package_name: String, secret hex_api_key: String) {
  use response <- result.try({
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path("/api/packages/" <> package_name)
    |> request.prepend_header("authorization", hex_api_key)
    |> request.prepend_header("user-agent", "gloogle / 1.0.0")
    |> httpc.send()
    |> result.map_error(error.HttpcError)
  })

  response.body
  |> json.parse(using: hexpm.package_decoder())
  |> result.map_error(error.JsonError)
}

fn hex_owner_decoder() {
  use username <- decode.field("username", decode.string)
  use email <- decrypt.optional_field("email", decode.string)
  use url <- decode.field("url", decode.string)
  decode.success(hexpm.PackageOwner(username:, email:, url:))
}

pub fn lookup_release(release: hexpm.PackageRelease, secret hex_api_key: String) {
  let assert Ok(url) = uri.parse(release.url)
  use response <- result.try({
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path(url.path)
    |> request.prepend_header("authorization", hex_api_key)
    |> request.prepend_header("user-agent", "gloogle / 1.0.0")
    |> httpc.send()
    |> result.map_error(error.HttpcError)
  })

  response.body
  |> json.parse(using: hexpm.release_decoder())
  |> result.map_error(error.JsonError)
}

pub fn get_api_packages_page(page: Int, hex_api_key: String) {
  let page = int.to_string(page)
  use response <- result.try({
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path("/api/packages")
    |> request.prepend_header("authorization", hex_api_key)
    |> request.prepend_header("user-agent", "gloogle / 1.0.0")
    |> request.set_query([
      #("sort", "updated_at"),
      #("page", page),
      #("search", "build_tool:gleam"),
    ])
    |> httpc.send()
    |> result.map_error(error.HttpcError)
  })

  response.body
  |> json.parse(using: decode.list(of: hexpm.package_decoder()))
  |> result.map_error(error.JsonError)
}
