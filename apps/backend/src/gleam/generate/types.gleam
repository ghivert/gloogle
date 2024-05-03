import backend/index/error
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/package_interface.{
  type Parameter, type Type, type TypeConstructor, type TypeDefinition,
}
import gleam/pair
import gleam/pgo
import gleam/result
import gleam/set
import gleam/verl
import tom.{type Toml}

type GleamToml =
  Dict(String, Toml)

pub fn type_definition_to_json(
  db: pgo.Connection,
  type_name: String,
  type_def: TypeDefinition,
  gleam_toml: GleamToml,
) -> Result(#(Json, List(Int)), error.Error) {
  use #(constructors, parameters) <- result.try({
    type_def.constructors
    |> list.fold_right(Ok(#([], set.new())), fn(acc, val) {
      use #(jsons, all_params) <- result.try(acc)
      use #(json, new_params) <- result.try(type_constructor_to_json(
        db,
        gleam_toml,
        val,
      ))
      Ok(#([json, ..jsons], set.union(of: new_params, and: all_params)))
    })
  })
  Ok(#(
    json.object([
      #("type", json.string("type-definition")),
      #("name", json.string(type_name)),
      #("documentation", json.nullable(type_def.documentation, json.string)),
      #("deprecation", json.nullable(type_def.documentation, json.string)),
      #("parameters", json.int(type_def.parameters)),
      #("constructors", json.preprocessed_array(constructors)),
    ]),
    parameters
      |> set.to_list(),
  ))
}

fn type_constructor_to_json(
  db: pgo.Connection,
  gleam_toml: GleamToml,
  constructor: TypeConstructor,
) {
  use #(elements, params) <- result.try({
    constructor.parameters
    |> list.fold_right(Ok(#([], set.new())), fn(acc, val) {
      use #(jsons, all_params) <- result.try(acc)
      use #(json, new_params) <- result.try(parameters_to_json(
        db,
        gleam_toml,
        val,
      ))
      Ok(#([json, ..jsons], set.union(of: new_params, and: all_params)))
    })
  })
  Ok(#(
    json.object([
      #("type", json.string("type-constructor")),
      #("documentation", json.nullable(constructor.documentation, json.string)),
      #("name", json.string(constructor.name)),
      #("parameters", json.preprocessed_array(elements)),
    ]),
    params,
  ))
}

fn parameters_to_json(
  db: pgo.Connection,
  gleam_toml: GleamToml,
  parameter: Parameter,
) {
  use #(type_, params) <- result.try(type_to_json(
    db,
    gleam_toml,
    parameter.type_,
  ))
  Ok(#(
    json.object([
      #("type", json.string("parameter")),
      #("label", json.nullable(parameter.label, json.string)),
      #("type", type_),
    ]),
    params,
  ))
}

fn type_to_json(db: pgo.Connection, gleam_toml: GleamToml, type_: Type) {
  case type_ {
    package_interface.Tuple(els) -> {
      use #(elements, params) <- result.try({
        els
        |> list.fold_right(Ok(#([], set.new())), fn(acc, val) {
          use #(jsons, all_params) <- result.try(acc)
          use #(json, new_params) <- result.try(type_to_json(
            db,
            gleam_toml,
            val,
          ))
          Ok(#([json, ..jsons], set.union(of: new_params, and: all_params)))
        })
      })
      Ok(#(
        json.object([
          #("type", json.string("tuple")),
          #("elements", json.preprocessed_array(elements)),
        ]),
        params,
      ))
    }
    package_interface.Fn(params, return) -> {
      use #(elements, params) <- result.try({
        params
        |> list.fold_right(Ok(#([], set.new())), fn(acc, val) {
          use #(jsons, all_params) <- result.try(acc)
          use #(json, new_params) <- result.try(type_to_json(
            db,
            gleam_toml,
            val,
          ))
          Ok(#([json, ..jsons], set.union(of: new_params, and: all_params)))
        })
      })
      use #(return_json, return_params) <- result.try(type_to_json(
        db,
        gleam_toml,
        return,
      ))
      Ok(#(
        json.object([
          #("type", json.string("fn")),
          #("params", json.preprocessed_array(elements)),
          #("return", return_json),
        ]),
        set.union(of: params, and: return_params),
      ))
    }
    package_interface.Variable(id) ->
      Ok(#(
        json.object([#("type", json.string("variable")), #("id", json.int(id))]),
        set.new(),
      ))
    package_interface.Named(name, package, module, parameters) -> {
      use #(elements, params) <- result.try({
        parameters
        |> list.fold_right(Ok(#([], set.new())), fn(acc, val) {
          use #(jsons, all_params) <- result.try(acc)
          use #(json, new_params) <- result.try(type_to_json(
            db,
            gleam_toml,
            val,
          ))
          Ok(#([json, ..jsons], set.union(of: new_params, and: all_params)))
        })
      })
      use ref <- result.map(case is_prelude(package, module) {
        True -> Ok(-1)
        False -> {
          use requirement <- result.try(get_toml_requirement(
            gleam_toml,
            package,
          ))
          use release <- result.try({
            "SELECT package_release.id, package_release.version
             FROM package
             JOIN package_release
               ON package.id = package_release.package_id
             WHERE package.name = $1"
            |> pgo.execute(
              db,
              [pgo.text(package)],
              dynamic.tuple2(dynamic.int, dynamic.string),
            )
            |> result.map_error(error.DatabaseError)
            |> result.try(fn(r) {
              r.rows
              |> list.fold(None, fn(acc, val) {
                let requirement = bit_array.from_string(requirement)
                let bit_version =
                  val
                  |> pair.second()
                  |> bit_array.from_string()
                let is_matching =
                  verl.is_match(version: bit_version, requirement: requirement)
                use <- bool.guard(when: !is_matching, return: acc)
                acc
                |> option.map(fn(saved) {
                  case
                    verl.gte(
                      version: saved
                        |> pair.second()
                        |> bit_array.from_string(),
                      with: bit_version,
                    )
                  {
                    True -> saved
                    False -> val
                  }
                })
                |> option.or(Some(val))
              })
              |> option.map(fn(v) { pair.first(v) })
              |> option.to_result(error.UnknownError("Release not found"))
            })
          })
          use module <- result.try({
            "SELECT signature.id FROM package_type_fun_signature signature JOIN package_module ON signature.package_module_id = package_module.id WHERE package_module.name = $1 AND package_module.package_release_id = $2"
            |> pgo.execute(
              db,
              [pgo.text(module), pgo.int(release)],
              dynamic.element(0, dynamic.int),
            )
            |> result.map_error(error.DatabaseError)
          })
          module.rows
          |> list.first()
          |> result.replace_error(error.UnknownError("No type defined"))
        }
      })
      #(
        json.object([
          #("type", json.string("named")),
          #("ref", json.int(ref)),
          #("name", json.string(name)),
          #("package", json.string(package)),
          #("module", json.string(module)),
          #("parameters", json.preprocessed_array(elements)),
        ]),
        set.insert(params, ref),
      )
    }
  }
}

fn get_toml_requirement(gleam_toml: GleamToml, package: String) {
  tom.get_string(gleam_toml, ["name"])
  |> result.try(fn(package_name) {
    let not_same_package = package_name != package
    use <- bool.guard(when: not_same_package, return: Error(tom.NotFound([])))
    tom.get_string(gleam_toml, ["version"])
  })
  |> result.try_recover(fn(_) {
    tom.get_string(gleam_toml, ["dependencies", package])
  })
  |> result.replace_error(error.UnknownError("No dep found"))
}

fn is_prelude(package: String, module: String) {
  module == "gleam" && package == ""
}
