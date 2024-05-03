import backend/index/error
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/function
import gleam/generate/sources.{
  constant_to_string, function_to_string, type_alias_to_string,
  type_definition_to_string,
}
import gleam/generate/types.{
  constant_to_json, function_to_json, implementations_to_json,
  type_alias_to_json, type_definition_to_json,
}
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/package_interface.{type Module, type Package}
import gleam/pgo
import gleam/result
import gleam/string
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
  |> result.replace_error(error.UnknownError(
    "No release found for " <> package.name <> "@" <> package.version,
  ))
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
  |> result.replace_error(error.UnknownError(
    "No module found for " <> module_name,
  ))
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
    let deprecation = option.map(type_def.deprecation, fn(d) { d.message })
    let metadata =
      json.object([#("deprecation", json.nullable(deprecation, json.string))])
      |> json.to_string()
    let signature = type_definition_to_string(type_name, type_def)
    let type_def_json =
      type_definition_to_json(db, type_name, type_def, gleam_toml)
    use #(json_signature, parameters) <- result.try(type_def_json)

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
        dynamic.unsafe_coerce(dynamic.from(parameters)),
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

fn upsert_type_aliases(
  db: pgo.Connection,
  module_id: Int,
  module: Module,
  gleam_toml: Dict(String, Toml),
) {
  let all_types = dict.to_list(module.type_aliases)
  result.all({
    use #(type_name, type_alias) <- list.map(all_types)
    let documentation = string.trim(option.unwrap(type_alias.documentation, ""))
    let deprecation = option.map(type_alias.deprecation, fn(d) { d.message })
    let metadata =
      json.object([#("deprecation", json.nullable(deprecation, json.string))])
      |> json.to_string()
    let signature = type_alias_to_string(type_name, type_alias)
    let type_alias_json =
      type_alias_to_json(db, type_name, type_alias, gleam_toml)
    use #(json_signature, parameters) <- result.try(type_alias_json)

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
   ) VALUES ($1, $2, $3, $4, 'type_alias', $5, $6, $7, $8)
   ON CONFLICT (package_module_id, name) DO UPDATE
     SET
       documentation = $2,
       signature_ = $3,
       json_signature = $4,
       nature = 'type_alias',
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
        dynamic.unsafe_coerce(dynamic.from(parameters)),
        pgo.text(metadata),
        pgo.int(module_id),
        type_alias.deprecation
          |> option.map(fn(d) { d.message })
          |> pgo.nullable(pgo.text, _),
      ],
      dynamic.dynamic,
    )
    |> result.map_error(error.DatabaseError)
    |> result.replace(Nil)
  })
}

fn implementations_pgo(implementations: package_interface.Implementations) {
  [
    #("gleam", implementations.gleam),
    #("erlang", implementations.uses_erlang_externals),
    #("javascript", implementations.uses_javascript_externals),
  ]
  |> list.filter(fn(t) { t.1 })
  |> list.map(fn(t) { t.0 })
  |> string.join(",")
}

fn upsert_constants(
  db: pgo.Connection,
  module_id: Int,
  module: Module,
  gleam_toml: Dict(String, Toml),
) {
  let all_constants = dict.to_list(module.constants)
  result.all({
    use #(constant_name, constant) <- list.map(all_constants)
    let documentation = string.trim(option.unwrap(constant.documentation, ""))
    let deprecation = option.map(constant.deprecation, fn(d) { d.message })
    let impl = constant.implementations
    let metadata =
      json.object([
        #("deprecation", json.nullable(deprecation, json.string)),
        #("implementations", implementations_to_json(impl)),
      ])
      |> json.to_string()
    let signature = constant_to_string(constant_name, constant)
    let constant_json =
      constant_to_json(db, constant_name, constant, gleam_toml)
    use #(json_signature, parameters) <- result.try(constant_json)

    "INSERT INTO package_type_fun_signature (
     name,
     documentation,
     signature_,
     json_signature,
     nature,
     parameters,
     metadata,
     package_module_id,
     deprecation,
     implementations
   ) VALUES ($1, $2, $3, $4, 'constant', $5, $6, $7, $8, $9)
   ON CONFLICT (package_module_id, name) DO UPDATE
     SET
       documentation = $2,
       signature_ = $3,
       json_signature = $4,
       nature = 'constant',
       parameters = $5,
       metadata = $6,
       deprecation = $8,
       implementations = $9"
    |> pgo.execute(
      db,
      [
        pgo.text(constant_name),
        pgo.text(documentation),
        pgo.text(signature),
        json_signature
          |> json.to_string()
          |> pgo.text(),
        dynamic.unsafe_coerce(dynamic.from(parameters)),
        pgo.text(metadata),
        pgo.int(module_id),
        constant.deprecation
          |> option.map(fn(d) { d.message })
          |> pgo.nullable(pgo.text, _),
        constant.implementations
          |> implementations_pgo()
          |> pgo.text(),
      ],
      dynamic.dynamic,
    )
    |> result.map_error(error.DatabaseError)
    |> result.replace(Nil)
  })
}

fn upsert_functions(
  db: pgo.Connection,
  module_id: Int,
  module: Module,
  gleam_toml: Dict(String, Toml),
) {
  let all_functions = dict.to_list(module.functions)
  result.all({
    use #(function_name, function) <- list.map(all_functions)
    let documentation = string.trim(option.unwrap(function.documentation, ""))
    let deprecation = option.map(function.deprecation, fn(d) { d.message })
    let impl = function.implementations
    let metadata =
      json.object([
        #("deprecation", json.nullable(deprecation, json.string)),
        #("implementations", implementations_to_json(impl)),
      ])
      |> json.to_string()
    let signature = function_to_string(function_name, function)
    let function_json =
      function_to_json(db, function_name, function, gleam_toml)
    use #(json_signature, parameters) <- result.try(function_json)

    "INSERT INTO package_type_fun_signature (
     name,
     documentation,
     signature_,
     json_signature,
     nature,
     parameters,
     metadata,
     package_module_id,
     deprecation,
     implementations
   ) VALUES ($1, $2, $3, $4, 'function', $5, $6, $7, $8, $9)
   ON CONFLICT (package_module_id, name) DO UPDATE
     SET
       documentation = $2,
       signature_ = $3,
       json_signature = $4,
       nature = 'function',
       parameters = $5,
       metadata = $6,
       deprecation = $8,
       implementations = $9"
    |> pgo.execute(
      db,
      [
        pgo.text(function_name),
        pgo.text(documentation),
        pgo.text(signature),
        json_signature
          |> json.to_string()
          |> pgo.text(),
        dynamic.unsafe_coerce(dynamic.from(parameters)),
        pgo.text(metadata),
        pgo.int(module_id),
        function.deprecation
          |> option.map(fn(d) { d.message })
          |> pgo.nullable(pgo.text, _),
        function.implementations
          |> implementations_pgo()
          |> pgo.text(),
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
  wisp.log_info(
    "Extracting signatures for " <> package.name <> "@" <> package.version,
  )
  use #(_pid, rid) <- result.try(get_package_release_ids(db, package))
  use _ <- result.try(add_gleam_constraint(db, package, rid))
  package.modules
  |> dict.to_list()
  |> list.map(fn(mod) {
    let #(mod_name, module) = mod
    let qualified_name =
      package.name <> "/" <> mod_name <> "@" <> package.version
    wisp.log_info("Extracting signatures for " <> qualified_name)
    use module_id <- result.try(upsert_package_module(db, mod_name, module, rid))
    wisp.log_info("Extracting " <> qualified_name <> " type definitions")
    use _ <- result.try(upsert_type_definitions(
      db,
      module_id,
      module,
      gleam_toml,
    ))
    wisp.log_info("Extracting " <> qualified_name <> " type aliases")
    use _ <- result.try(upsert_type_aliases(db, module_id, module, gleam_toml))
    wisp.log_info("Extracting " <> qualified_name <> " constants")
    use _ <- result.try(upsert_constants(db, module_id, module, gleam_toml))
    wisp.log_info("Extracting " <> qualified_name <> " functions")
    upsert_functions(db, module_id, module, gleam_toml)
    |> function.tap(fn(r) {
      use <- bool.guard(when: result.is_error(r), return: Nil)
      wisp.log_info("Extracting " <> qualified_name <> " finished")
    })
  })
  |> result.all()
}
