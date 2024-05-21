import backend/error
import gleam/bit_array
import gleam/erlang/process
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/package_interface
import gleam/result
import s3
import simplifile
import tom
import wisp

@external(erlang, "gloogle_hex_ffi", "get_home")
pub fn get_home() -> Result(String, Nil)

@external(erlang, "gloogle_hex_ffi", "extract_tar")
fn extract_tar(
  tarbin: BitArray,
  base_name: String,
  slug: String,
) -> Result(#(String, String, String), Nil)

@external(erlang, "gloogle_hex_ffi", "remove_tar")
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
  let _ = put_s3(name, slug, content)
  wisp.log_debug("Using filesystem for " <> slug)
  content
}

fn create_archive(
  archives_path: String,
  name: String,
  version: String,
  archive: BitArray,
) {
  let slug = package_slug(name, version) <> ".tar"
  let package_path = archives_path <> "/" <> name
  let _ = simplifile.create_directory_all(package_path)
  let filepath = package_path <> "/" <> slug
  let _ = simplifile.write_bits(filepath, archive)
  let _ = put_s3(name, slug, archive)
  archive
}

fn read_s3(name: String, slug: String) {
  let full_slug = name <> "/" <> slug
  use archive <- result.map(s3.get(full_slug))
  wisp.log_debug("Using S3 for " <> slug)
  archive
}

fn put_s3(name: String, slug: String, archive: BitArray) {
  let full_slug = name <> "/" <> slug
  use _ <- result.map(s3.put(full_slug, archive))
  wisp.log_debug("Put on S3 for " <> slug)
  archive
}

fn get_tarball(name: String, version: String) {
  let slug = package_slug(name, version) <> ".tar"
  use archives_path <- result.try(create_archives_directory())
  use _ <- result.try_recover(read_archive(archives_path, name, version))
  use _ <- result.try_recover(read_s3(name, slug))
  wisp.log_debug("Querying hex for " <> slug)
  request.new()
  |> request.set_host("repo.hex.pm")
  |> request.set_path("/tarballs/" <> slug)
  |> request.set_method(http.Get)
  |> request.set_body(bit_array.from_string(""))
  |> request.set_scheme(http.Https)
  |> httpc.send_bits()
  |> result.map_error(error.FetchError)
  |> result.try(fn(res) {
    case res.status {
      200 -> Ok(create_archive(archives_path, name, version, res.body))
      _ -> {
        process.sleep(1000)
        get_tarball(name, version)
      }
    }
  })
}

fn read_interface(filepath: String, artifacts: String) {
  filepath
  |> simplifile.read()
  |> result.map_error(fn(error) {
    wisp.log_warning("Unable to read " <> filepath)
    wisp.log_warning("Compilation artifacts:")
    wisp.log_warning(artifacts)
    error.SimplifileError(error, filepath)
  })
}

fn read_toml_file(filepath: String) {
  filepath
  |> simplifile.read()
  |> result.map_error(error.SimplifileError(_, filepath))
}

fn read_package_interface(blob: String) {
  blob
  |> json.decode(using: package_interface.decoder)
  |> result.map_error(error.JsonError)
}

fn parse_toml(toml_blob: String) {
  tom.parse(toml_blob)
  |> result.map_error(error.ParseTomlError)
}

fn extract_package_infos(name: String, version: String) {
  let package_name = name <> "@" <> version
  let slug = package_slug(name, version)
  let req = get_tarball(name, version)
  use body <- result.try(req)
  use #(interface_s, toml_s, res) <- result.try({
    body
    |> extract_tar(name, slug)
    |> result.map_error(fn(_) {
      let content = "Impossible to extract tar for " <> package_name
      wisp.log_warning(content)
      error.UnknownError(content)
    })
  })
  use interface_blob <- result.try(read_interface(interface_s, res))
  use toml_blob <- result.try(read_toml_file(toml_s))
  use #(i, t) <- result.map(parse_files(interface_blob, toml_blob))
  #(i, t, interface_blob, toml_blob)
}

pub fn parse_files(interface: String, toml: String) {
  use interface <- result.try(read_package_interface(interface))
  use toml <- result.map(parse_toml(toml))
  #(interface, toml)
}

pub fn get_package_infos(name: String, version: String) {
  let slug = package_slug(name, version)
  let res = extract_package_infos(name, version)
  remove_tar(slug)
  res
}
