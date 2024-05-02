import backend/index/error
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/int
import gleam/iterator
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/package_interface.{
  type Module, type Package, type Parameter, type Type, type TypeConstructor,
  type TypeDefinition,
}
import gleam/pgo
import gleam/result
import gleam/string
import pprint
import ranger
import tom.{type Toml}

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

fn generate_type_definition_signature(
  type_name: String,
  type_def: TypeDefinition,
) {
  let params = case type_def.parameters {
    0 -> ""
    _ -> {
      let range =
        ranger.create_infinite(
          validate: fn(a) { string.length(a) == 1 },
          add: fn(a: String, b: Int) {
            let assert [code] = string.to_utf_codepoints(a)
            let int_code = string.utf_codepoint_to_int(code)
            let new_int_code = int_code + b
            let assert Ok(new_code) = string.utf_codepoint(new_int_code)
            string.from_utf_codepoints([new_code])
          },
          compare: string.compare,
        )
      let assert Ok(from_a) = range("a", 1)
      from_a
      |> iterator.take(type_def.parameters)
      |> iterator.to_list()
      |> string.join(", ")
      |> fn(s) { "(" <> s <> ")" }
    }
  }
  let base = type_name <> params
  case type_def.constructors {
    [] -> base
    items -> base <> " {\n" <> generate_type_constructors(items) <> "\n}"
  }
}

fn generate_type_constructors(constructors: List(TypeConstructor)) {
  constructors
  |> list.map(fn(c) {
    let parameters = generate_parameters(c.parameters)
    let const_ = "  " <> c.name <> parameters
    case c.documentation {
      None -> const_
      Some(d) -> string.join(["  -- " <> d, const_], "\n")
    }
  })
  |> string.join("\n")
}

fn generate_parameters(parameters: List(Parameter)) {
  use <- bool.guard(when: list.is_empty(parameters), return: "")
  parameters
  |> list.map(fn(s) {
    let label =
      s.label
      |> option.map(string.append(_, ": "))
      |> option.unwrap("")
    label <> generate_type(s.type_)
  })
  |> string.join(", ")
  |> fn(s) { "(" <> s <> ")" }
}

fn generate_type(type_: Type) {
  case type_ {
    package_interface.Tuple(elements) -> {
      let els =
        elements
        |> list.map(generate_type)
        |> string.join(", ")
      "#(" <> els <> ")"
    }
    package_interface.Fn(parameters, return) -> {
      let ret = generate_type(return)
      let params =
        parameters
        |> list.map(generate_type)
        |> string.join(", ")
      "fn(" <> params <> ") -> " <> ret
    }
    package_interface.Variable(id) -> {
      let assert Ok(utf_a) =
        "a"
        |> string.to_utf_codepoints()
        |> list.first()
      { string.utf_codepoint_to_int(utf_a) + id }
      |> string.utf_codepoint()
      |> result.map(list.prepend([], _))
      |> result.map(string.from_utf_codepoints)
      |> result.unwrap("a")
    }
    package_interface.Named(name, package, module, parameters) -> {
      let params =
        parameters
        |> list.map(generate_type)
        |> string.join(", ")
        |> fn(s) {
          use <- bool.guard(when: string.is_empty(s), return: s)
          "(" <> s <> ")"
        }
      case package {
        "" -> name <> params
        _ -> module <> "." <> name <> params
      }
    }
  }
}

fn generate_type_definition_json_signature(
  type_name: String,
  type_def: TypeDefinition,
) -> Result(#(Json, List(Int)), error.Error) {
  Ok(#(json.object([]), []))
}

pub fn extract_signatures(
  db: pgo.Connection,
  package: Package,
  toml: Dict(String, Toml),
) {
  use #(_pid, rid) <- result.try(get_package_release_ids(db, package))
  use _ <- result.try(add_gleam_constraint(db, package, rid))
  package.modules
  |> dict.map_values(fn(mod_name, module) {
    pprint.debug("Inserting " <> mod_name)
    use module_id <- result.try(upsert_package_module(db, mod_name, module, rid))
    module.types
    |> dict.map_values(fn(type_name, type_def) {
      let documentation =
        option.unwrap(type_def.documentation, "")
        |> string.trim()
      let metadata =
        type_def.deprecation
        |> option.map(fn(d) {
          json.object([#("deprecation", json.string(d.message))])
        })
        |> option.map(json.to_string)
        |> option.unwrap("{}")
      let signature = generate_type_definition_signature(type_name, type_def)
      use #(json_signature, parameters) <- result.try(
        generate_type_definition_json_signature(type_name, type_def),
      )
      let params =
        parameters
        |> list.map(int.to_string)
        |> string.join(", ")
        |> fn(v) { "[" <> v <> "]" }
      pprint.debug(signature)
      Ok(Nil)
    }// "INSERT INTO package_type_fun_signature (name, documentation, signature_, json_signature, nature, parameters, metadata, package_module_id)
    //  VALUES ($1, $2, $3, $4, 'type_definition', $5, $6, $7)
    //  ON CONFLICT (package_module_id, name) DO UPDATE
    //    SET documentation = $2, signature_ = $3, json_signature = $4, nature = 'type_definition', parameters = $5, metadata = $6"
    // |> pgo.execute(
    //   db,
    //   [
    //     pgo.text(type_name),
    //     pgo.text(documentation),
    //     pgo.text(signature),
    //     json_signature
    //       |> json.to_string()
    //       |> pgo.text(),
    //     pgo.text(params),
    //     pgo.text(metadata),
    //     pgo.int(module_id),
    //   ],
    //   dynamic.dynamic,
    // )
    // |> result.map_error(error.DatabaseError)
    )
    Ok(Nil)
  })
  Ok(Nil)
}
