import backend/error
import gleam/bit_array
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/option
import gleam/package_interface
import gleam/result
import simplifile
import tom

@external(erlang, "gling_hex_ffi", "extract_tar")
fn extract_tar(
  tarbin: BitArray,
  base_name: String,
  slug: String,
) -> #(String, String)

@external(erlang, "gling_hex_ffi", "remove_tar")
fn remove_tar(slug: String) -> Nil

fn package_slug(name: String, version: String) {
  name <> "-" <> version
}

fn get_tarball(name: String, version: String) {
  let slug = package_slug(name, version) <> ".tar"
  request.new()
  |> request.set_host("repo.hex.pm")
  |> request.set_path("/tarballs/" <> slug)
  |> request.set_method(http.Get)
  |> request.set_body(bit_array.from_string(""))
  |> request.set_scheme(http.Https)
  |> httpc.send_bits()
  |> result.map_error(error.FetchError)
}

fn read_interface(filepath: String) {
  case simplifile.read(filepath) {
    Ok(interface) -> Ok(option.Some(interface))
    Error(_) -> Ok(option.None)
  }
}

fn read_file(filepath: String) {
  filepath
  |> simplifile.read()
  |> result.map_error(error.SimplifileError(_, filepath))
}

fn read_package_interface(blob: option.Option(String)) {
  case blob {
    option.None -> Ok(option.None)
    option.Some(blob) ->
      blob
      |> json.decode(using: package_interface.decoder)
      |> result.map_error(error.JsonError)
      |> result.map(option.Some)
  }
}

fn read_gleam_toml(blob: String) {
  blob
  |> tom.parse()
  |> result.map_error(error.ParseTomlError)
}

fn extract_package_infos(name: String, version: String) {
  let slug = package_slug(name, version)
  let req = get_tarball(name, version)
  use res <- result.try(req)
  let #(interface, toml) = extract_tar(res.body, name, slug)
  use interface_blob <- result.try(read_interface(interface))
  use toml_blob <- result.try(read_file(toml))
  use interface <- result.try(read_package_interface(interface_blob))
  use toml <- result.map(read_gleam_toml(toml_blob))
  #(interface, toml)
}

pub fn get_package_infos(name: String, version: String) {
  let slug = package_slug(name, version)
  let res = extract_package_infos(name, version)
  remove_tar(slug)
  res
}
