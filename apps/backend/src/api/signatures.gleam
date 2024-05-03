import backend/index/error
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/generate/sources.{type_definition_to_string}
import gleam/generate/types.{type_definition_to_json}
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/package_interface.{type Module, type Package}
import gleam/pgo
import gleam/result
import gleam/string
import pprint
import tom.{type Toml}
import wisp

fn add_gleam_constraint(db: pgo.Connection, package: Package, release_id: Int) {
  case package.gleam_version_constraint {
    None -> Ok(Nil)
    Some(c) -> {
      "UPDATE package_release SET gleam_constraint = $1 WHERE id = $2"
      |> pgo.execute(db, [pgo.text(c), pgo.int(release_id)], dynamic.dynamic)
      |> result.replace(Nil)
      |> result.map_error(error.DatabaseError)
    }
  }
}

fn get_package_release_ids(db: pgo.Connection, package: Package) {
  use response <- result.try({
    let args = [pgo.text(package.name), pgo.text(package.version)]
    "SELECT
       package.id package_id,
       package_release.id package_release_id
     FROM package
     JOIN package_release
       ON package_release.package_id = package.id
     WHERE package.name = $1
       AND package_release.version = $2"
    |> pgo.execute(db, args, dynamic.tuple2(dynamic.int, dynamic.int))
    |> result.map_error(error.DatabaseError)
  })
  response.rows
  |> list.first()
  |> result.replace_error(error.UnknownError("No release"))
}

fn upsert_package_module(
  db: pgo.Connection,
  module_name: String,
  module: Module,
  release_id: Int,
) {
  let documentation =
    module.documentation
    |> string.join("\n")
    |> pgo.text()
  use response <- result.try({
    let args = [pgo.text(module_name), documentation, pgo.int(release_id)]
    "INSERT INTO package_module (name, documentation, package_release_id)
     VALUES ($1, $2, $3)
     ON CONFLICT (name, package_release_id) DO UPDATE
       SET documentation = $2
     RETURNING id"
    |> pgo.execute(db, args, dynamic.element(0, dynamic.int))
    |> result.map_error(error.DatabaseError)
  })
  response.rows
  |> list.first()
  |> result.replace_error(error.UnknownError("No module"))
}

fn upsert_type_definitions(
  db: pgo.Connection,
  module_id: Int,
  module: Module,
  gleam_toml: Dict(String, Toml),
) {
  let all_types = dict.to_list(module.types)
  result.all({
    use #(type_name, type_def) <- list.map(all_types)
    let documentation = string.trim(option.unwrap(type_def.documentation, ""))
    let metadata =
      type_def.deprecation
      |> option.map(fn(d) { json.string(d.message) })
      |> option.map(fn(d) { json.object([#("deprecation", d)]) })
      |> option.map(json.to_string)
      |> option.unwrap("{}")
    let signature = type_definition_to_string(type_name, type_def)
    let type_def_json =
      type_definition_to_json(db, type_name, type_def, gleam_toml)
    use #(json_signature, parameters) <- result.try(type_def_json)
    pprint.debug(
      json_signature
      |> json.to_string(),
    )
    let params = parameters

    "INSERT INTO package_type_fun_signature (
     name,
     documentation,
     signature_,
     json_signature,
     nature,
     parameters,
     metadata,
     package_module_id,
     deprecation
   ) VALUES ($1, $2, $3, $4, 'type_definition', $5, $6, $7, $8)
   ON CONFLICT (package_module_id, name) DO UPDATE
     SET
       documentation = $2,
       signature_ = $3,
       json_signature = $4,
       nature = 'type_definition',
       parameters = $5,
       metadata = $6,
       deprecation = $8"
    |> pgo.execute(
      db,
      [
        pgo.text(type_name),
        pgo.text(documentation),
        pgo.text(signature),
        json_signature
          |> json.to_string()
          |> pgo.text(),
        dynamic.unsafe_coerce(dynamic.from(params)),
        pgo.text(metadata),
        pgo.int(module_id),
        type_def.deprecation
          |> option.map(fn(d) { d.message })
          |> pgo.nullable(pgo.text, _),
      ],
      dynamic.dynamic,
    )
    |> result.map_error(error.DatabaseError)
    |> result.replace(Nil)
  })
}

pub fn extract_signatures(
  db: pgo.Connection,
  package: Package,
  gleam_toml: Dict(String, Toml),
) {
  use #(_pid, rid) <- result.try(get_package_release_ids(db, package))
  use _ <- result.try(add_gleam_constraint(db, package, rid))
  package.modules
  |> dict.to_list()
  |> list.map(fn(mod) {
    let #(mod_name, module) = mod
    wisp.log_info("Inserting " <> mod_name)
    use module_id <- result.try(upsert_package_module(db, mod_name, module, rid))
    module
    |> upsert_type_definitions(db, module_id, _, gleam_toml)
    |> result.replace(Nil)
  })
  |> result.all()
  |> io.debug()
}
