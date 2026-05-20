import aws/s3
import fs
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/erlang/os
import gleam/erlang/process
import gleam/function_
import gleam/http
import gleam/http/request
import gleam/http/response.{Response}
import gleam/httpc
import gleam/json
import gleam/package_interface
import gleam/result
import gleam/result_
import gleam/string
import jupiter/error
import palabres
import simplifile
import tar
import tom

const module = "api/hex_repo"

/// Get the tarball package from Hex, build the package, extract the
/// package-interface.json and the gleam.toml.
fn extract_tar(
  tarbin: BitArray,
  base_name: String,
  version: String,
  slug: String,
) -> Result(#(String, String, String), Nil) {
  let package_path = string.join(["/tmp", slug], with: "/")
  let destination = string.join([package_path, "contents"], with: "/")
  let content = string.join([package_path, "contents.tar.gz"], with: "/")
  use _ <- result.try(tar.extract_binary(tarbin, package_path))
  use _ <- result.try(tar.extract(content, destination))
  use request <- result.map(request_package_interface(base_name, version))
  let gleam_toml = string.join([destination, "gleam.toml"], with: "/")
  case httpc.send(request) {
    Ok(Response(status: 200, body:, ..)) -> {
      palabres.debug("Using HexDocs for package-interface.json")
      |> palabres.string("slug", slug)
      |> palabres.at(module:, function: "extract_tar")
      |> palabres.log
      let path = string.join([destination, "package-interface.json"], with: "/")
      let _ = fs.write_file(path, body)
      #(path, gleam_toml, "")
    }
    Ok(_) | Error(_) -> {
      let build_cmd = "cd " <> destination <> " && gleam docs build"
      let res = os.cmd(build_cmd)
      #(package_interface_path(destination, base_name), gleam_toml, res)
    }
  }
}

fn request_package_interface(name: String, version: String) {
  ["https://hexdocs.pm", name, version, "package-interface.json"]
  |> string.join(with: "/")
  |> request.to
}

fn package_interface_path(destination, base_name) {
  let build_folder = "/build/dev/docs/"
  let package = "/package-interface.json"
  destination <> build_folder <> base_name <> package
}

fn package_slug(name: String, version: String) {
  name <> "-" <> version
}

fn create_archives_directory() {
  fs.home()
  |> result.replace_error(error.CustomError("home not found"))
  |> result.map(fn(home) {
    let archives_path = string.join([home, "archives/gleam"], with: "/")
    let _ = simplifile.create_directory_all(archives_path)
    archives_path
  })
}

fn read_archive(archives_path: String, name: String, version: String) {
  let slug = package_slug(name, version) <> ".tar"
  let filepath = string.join([archives_path, name, slug], with: "/")
  use content <- result_.tap(simplifile.read_bits(filepath))
  palabres.debug("[hex_repo] Using filesystem")
  |> palabres.string("slug", slug)
  |> palabres.at(module:, function: "read_archive")
  |> palabres.log
  put_s3(name, slug, content)
}

fn create_archive(
  archives_path: String,
  name: String,
  version: String,
  archive: BitArray,
) -> BitArray {
  use _ <- function_.tap(archive)
  let slug = package_slug(name, version) <> ".tar"
  let package_path = string.join([archives_path, name], with: "/")
  let _ = simplifile.create_directory_all(package_path)
  let filepath = string.join([package_path, slug], with: "/")
  let _ = simplifile.write_bits(filepath, archive)
  let _ = put_s3(name, slug, archive)
}

fn read_s3(name: String, slug: String) {
  let full_slug = string.join([name, slug], with: "/")
  use _archive <- result_.tap(s3.get(full_slug))
  palabres.debug("[hex_repo] Using S3")
  |> palabres.string("slug", slug)
  |> palabres.at(module:, function: "read_s3")
}

fn put_s3(name: String, slug: String, archive: BitArray) {
  let full_slug = string.join([name, slug], with: "/")
  use _ <- result.map(s3.put(full_slug, archive))
  palabres.debug("Put on archive on S3")
  |> palabres.string("slug", slug)
  |> palabres.at(module:, function: "put_s3")
  |> palabres.log
  archive
}

fn get_tarball(name: String, version: String) {
  let slug = package_slug(name, version) <> ".tar"
  use archives_path <- result.try(create_archives_directory())
  use _ <- result.try_recover(read_archive(archives_path, name, version))
  use _ <- result.try_recover(read_s3(name, slug))
  palabres.debug("Querying tarball on Hex")
  |> palabres.string("slug", slug)
  |> palabres.at(module:, function: "get_tarball")
  |> palabres.log
  request.new()
  |> request.set_host("repo.hex.pm")
  |> request.set_path("/tarballs/" <> slug)
  |> request.set_method(http.Get)
  |> request.set_body(bit_array.from_string(""))
  |> request.set_scheme(http.Https)
  |> httpc.send_bits
  |> result.map_error(error.HttpcError)
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
  |> simplifile.read
  |> result.map_error(fn(error) {
    palabres.warning("Unable to read package-interface.json")
    |> palabres.string("filepath", filepath)
    |> palabres.string("artifacts", artifacts)
    |> palabres.at(module:, function: "read_interface")
    |> palabres.log
    error.SimplifileError(error, filepath)
  })
}

fn read_toml_file(filepath: String) {
  filepath
  |> simplifile.read
  |> result.map_error(fn(error) {
    palabres.warning("Unable to read gleam.toml")
    |> palabres.string("filepath", filepath)
    |> palabres.at(module:, function: "read_toml_file")
    |> palabres.log
    error.SimplifileError(error, filepath)
  })
}

fn read_package_interface(blob: String) {
  blob
  |> json.parse(using: package_interface.decoder())
  |> result.map_error(fn(error) {
    palabres.warning("Unable to decode package-interface.json")
    |> palabres.string("package_interface", blob)
    |> palabres.at(module:, function: "read_package_interface")
    |> palabres.log
    error.JsonError(error)
  })
}

fn parse_toml(toml: String) {
  tom.parse(toml)
  |> result.map_error(fn(error) {
    palabres.warning("Unable to parse gleam.toml")
    |> palabres.string("toml", toml)
    |> palabres.at(module:, function: "parse_toml")
    |> palabres.log
    error.TomlParseError(error)
  })
}

fn extract_package_infos(name: String, version: String) {
  let package_name = name <> "@" <> version
  let slug = package_slug(name, version)
  let req = get_tarball(name, version)
  use body <- result.try(req)
  use #(package_interface, gleam_toml_path, build_output) <- result.try({
    body
    |> extract_tar(name, version, slug)
    |> result.map_error(fn(_) {
      palabres.warning("Impossible to extract tar")
      |> palabres.string("package_name", package_name)
      |> palabres.at(module:, function: "extract_package_infos")
      |> palabres.log
      error.CustomError("Impossible to extract tar for " <> package_name)
    })
  })
  use interface <- result.try(read_interface(package_interface, build_output))
  use gleam_toml <- result.try(read_toml_file(gleam_toml_path))
  use #(package, toml) <- result.map(parse_files(interface, gleam_toml))
  PackageContent(package, toml, interface, gleam_toml)
}

pub fn parse_files(interface: String, toml: String) {
  use interface <- result.try(read_package_interface(interface))
  use toml <- result.map(parse_toml(toml))
  #(interface, toml)
}

pub type PackageContent {
  PackageContent(
    package: package_interface.Package,
    toml: Dict(String, tom.Toml),
    package_interface: String,
    gleam_toml: String,
  )
}

pub fn get_package_infos(name: String, version: String) {
  let slug = package_slug(name, version)
  let file_path = "/tmp/" <> slug
  use _ <- function_.tap(extract_package_infos(name, version))
  tar.remove(file_path)
}
