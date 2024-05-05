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
import wisp

@external(erlang, "gling_hex_ffi", "get_home")
pub fn get_home() -> Result(String, Nil)

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

fn create_archives_directory() {
  let home_error = error.UnknownError("home not found")
  let home = result.replace_error(get_home(), home_error)
  use home <- result.map(home)
  let archives_path = home <> "/archives/gleam"
  let _ = simplifile.create_directory_all(archives_path)
  archives_path
}

fn read_archive(archives_path: String, name: String, version: String) {
  let slug = package_slug(name, version) <> ".tar"
  let filepath = archives_path <> "/" <> name <> "/" <> slug
  use content <- result.map(simplifile.read_bits(filepath))
  wisp.log_info("Using filesystem for " <> slug)
  content
}

fn create_archive(
  archives_path: String,
  name: String,
  version: String,
  archive: BitArray,
) {
  let slug = package_slug(name, version) <> ".tar"
  let filepath = archives_path <> "/" <> name <> "/" <> slug
  let _ = simplifile.write_bits(filepath, archive)
  archive
}

fn get_tarball(name: String, version: String) {
  let slug = package_slug(name, version) <> ".tar"
  use archives_path <- result.try(create_archives_directory())
  use _ <- result.try_recover(read_archive(archives_path, name, version))
  wisp.log_info("Querying hex for " <> slug)
  request.new()
  |> request.set_host("repo.hex.pm")
  |> request.set_path("/tarballs/" <> slug)
  |> request.set_method(http.Get)
  |> request.set_body(bit_array.from_string(""))
  |> request.set_scheme(http.Https)
  |> httpc.send_bits()
  |> result.map_error(error.FetchError)
  |> result.map(fn(res) {
    create_archive(archives_path, name, version, res.body)
  })
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
  use body <- result.try(req)
  let #(interface, toml) = extract_tar(body, name, slug)
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
