import backend/index/error
import gleam/bit_array
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/json.{type Json}
import gleam/list
import gleam/option
import gleam/order
import gleam/package_interface.{
  type Constant, type Function, type Implementations, type Parameter, type Type,
  type TypeAlias, type TypeConstructor, type TypeDefinition,
}
import gleam/pair
import gleam/pgo
import gleam/result
import gleam/set.{type Set}
import gleam/verl
import tom.{type Toml}

type GleamToml =
  Dict(String, Toml)

fn reduce_components(
  components: List(a),
  mapper: fn(a) -> Result(#(Json, Set(Int)), error.Error),
) {
  let init = Ok(#([], set.new()))
  use acc, val <- list.fold_right(components, init)
  use #(constructors, old_ids) <- result.try(acc)
  use gen <- result.map(mapper(val))
  let #(constructor, new_ids) = gen
  #([constructor, ..constructors], set.union(of: new_ids, and: old_ids))
}

pub fn type_definition_to_json(
  db: pgo.Connection,
  type_name: String,
  type_def: TypeDefinition,
  toml: GleamToml,
) -> Result(#(Json, List(Int)), error.Error) {
  let mapper = type_constructor_to_json(db, toml, _)
  use gen <- result.map(reduce_components(type_def.constructors, mapper))
  use constructors <- pair.map_first(pair.map_second(gen, set.to_list))
  json.object([
    #("type", json.string("type-definition")),
    #("name", json.string(type_name)),
    #("documentation", json.nullable(type_def.documentation, json.string)),
    #("deprecation", json.nullable(type_def.documentation, json.string)),
    #("parameters", json.int(type_def.parameters)),
    #("constructors", json.preprocessed_array(constructors)),
  ])
}

fn type_constructor_to_json(
  db: pgo.Connection,
  gleam_toml: GleamToml,
  constructor: TypeConstructor,
) {
  let mapper = parameters_to_json(db, gleam_toml, _)
  use gen <- result.map(reduce_components(constructor.parameters, mapper))
  use parameters <- pair.map_first(gen)
  json.object([
    #("type", json.string("type-constructor")),
    #("documentation", json.nullable(constructor.documentation, json.string)),
    #("name", json.string(constructor.name)),
    #("parameters", json.preprocessed_array(parameters)),
  ])
}

fn parameters_to_json(
  db: pgo.Connection,
  gleam_toml: GleamToml,
  parameter: Parameter,
) {
  use gen <- result.map(type_to_json(db, gleam_toml, parameter.type_))
  use type_ <- pair.map_first(gen)
  json.object([
    #("type", json.string("parameter")),
    #("label", json.nullable(parameter.label, json.string)),
    #("type", type_),
  ])
}

fn type_to_json(db: pgo.Connection, gleam_toml: GleamToml, type_: Type) {
  case type_ {
    package_interface.Tuple(elements) -> {
      let mapper = type_to_json(db, gleam_toml, _)
      use gen <- result.map(reduce_components(elements, mapper))
      use elements <- pair.map_first(gen)
      json.object([
        #("type", json.string("tuple")),
        #("elements", json.preprocessed_array(elements)),
      ])
    }
    package_interface.Fn(params, return) -> {
      let mapper = type_to_json(db, gleam_toml, _)
      use #(elements, params) <- result.try(reduce_components(params, mapper))
      use gen <- result.map(type_to_json(db, gleam_toml, return))
      let new_params = set.union(of: params, and: gen.1)
      json.object([
        #("type", json.string("fn")),
        #("params", json.preprocessed_array(elements)),
        #("return", gen.0),
      ])
      |> pair.new(new_params)
    }
    package_interface.Variable(id) -> {
      let json =
        json.object([#("type", json.string("variable")), #("id", json.int(id))])
      Ok(#(json, set.new()))
    }
    package_interface.Named(name, package, module, parameters) -> {
      let mapper = type_to_json(db, gleam_toml, _)
      use gen <- result.try(reduce_components(parameters, mapper))
      use ref <- result.map(extract_parameters_relation(
        db,
        gleam_toml,
        name,
        package,
        module,
      ))
      let new_ids = set.insert(gen.1, ref)
      json.object([
        #("type", json.string("named")),
        #("ref", json.int(ref)),
        #("name", json.string(name)),
        #("package", json.string(package)),
        #("module", json.string(module)),
        #("parameters", json.preprocessed_array(gen.0)),
      ])
      |> pair.new(new_ids)
    }
  }
}

fn find_package_release(
  db: pgo.Connection,
  package: String,
  requirement: String,
) {
  let decoder = dynamic.tuple2(dynamic.int, dynamic.string)
  use response <- result.try({
    "SELECT package_release.id, package_release.version
     FROM package
     JOIN package_release
       ON package.id = package_release.package_id
     WHERE package.name = $1"
    |> pgo.execute(db, [pgo.text(package)], decoder)
    |> result.map_error(error.DatabaseError)
  })
  response.rows
  |> keep_matching_releases(requirement)
}

fn keep_matching_releases(rows: List(#(Int, String)), requirement: String) {
  let requirement = bit_array.from_string(requirement)
  rows
  |> list.filter(fn(r) {
    let version = bit_array.from_string(r.1)
    let is_matching = verl.is_match(version: version, requirement: requirement)
    result.unwrap(is_matching, False)
  })
  |> list.sort(fn(a, b) {
    let a = bit_array.from_string(a.1)
    let b = bit_array.from_string(b.1)
    case verl.gte(version: a, with: b) {
      True -> order.Lt
      False -> order.Gt
    }
  })
  |> list.map(fn(a) { a.0 })
  |> Ok()
}

fn find_type_signature(
  db: pgo.Connection,
  name: String,
  package: String,
  requirement: String,
  module: String,
  releases: List(Int),
) {
  use response <- result.try({
    list.fold(releases, option.None, fn(acc, release) {
      use <- bool.guard(when: option.is_some(acc), return: acc)
      case
        "SELECT signature.id
     FROM package_type_fun_signature signature
     JOIN package_module
       ON signature.package_module_id = package_module.id
     WHERE signature.name = $1
       AND package_module.name = $2
       AND package_module.package_release_id = $3"
        |> pgo.execute(
          db,
          [pgo.text(name), pgo.text(module), pgo.int(release)],
          dynamic.element(0, dynamic.int),
        )
      {
        Ok(value) -> option.Some(value)
        Error(_) -> option.None
      }
    })
    |> option.to_result(error.UnknownError(
      "Release "
      <> package
      <> " with conditions "
      <> requirement
      <> " not found",
    ))
  })
  response.rows
  |> list.first()
  |> result.replace_error(error.UnknownError(
    "No type found for " <> module <> "." <> name,
  ))
}

fn extract_parameters_relation(
  db: pgo.Connection,
  gleam_toml: GleamToml,
  name: String,
  package: String,
  module: String,
) {
  use <- bool.guard(when: is_prelude(package, module), return: Ok(-1))
  use requirement <- result.try(get_toml_requirement(gleam_toml, package))
  use releases <- result.try(find_package_release(db, package, requirement))
  find_type_signature(db, name, package, requirement, module, releases)
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
  |> result.replace_error(error.UnknownError("No dep found for " <> package))
}

fn is_prelude(package: String, module: String) {
  module == "gleam" && package == ""
}

pub fn type_alias_to_json(
  db: pgo.Connection,
  type_name: String,
  type_alias: TypeAlias,
  gleam_toml: GleamToml,
) {
  use gen <- result.map(type_to_json(db, gleam_toml, type_alias.alias))
  use alias <- pair.map_first(pair.map_second(gen, set.to_list))
  json.object([
    #("type", json.string("type-alias")),
    #("name", json.string(type_name)),
    #("documentation", json.nullable(type_alias.documentation, json.string)),
    #("deprecation", json.nullable(type_alias.documentation, json.string)),
    #("parameters", json.int(type_alias.parameters)),
    #("alias", alias),
  ])
}

pub fn implementations_to_json(implementations: Implementations) {
  json.object([
    #("gleam", json.bool(implementations.gleam)),
    #("uses_erlang_externals", json.bool(implementations.uses_erlang_externals)),
    #(
      "uses_javascript_externals",
      json.bool(implementations.uses_javascript_externals),
    ),
  ])
}

pub fn constant_to_json(
  db: pgo.Connection,
  constant_name: String,
  constant: Constant,
  gleam_toml: GleamToml,
) {
  use gen <- result.map(type_to_json(db, gleam_toml, constant.type_))
  use type_ <- pair.map_first(pair.map_second(gen, set.to_list))
  json.object([
    #("type", json.string("constant")),
    #("name", json.string(constant_name)),
    #("documentation", json.nullable(constant.documentation, json.string)),
    #("deprecation", json.nullable(constant.documentation, json.string)),
    #("implementations", implementations_to_json(constant.implementations)),
    #("type", type_),
  ])
}

pub fn function_to_json(
  db: pgo.Connection,
  function_name: String,
  function: Function,
  gleam_toml: GleamToml,
) {
  let mapper = parameters_to_json(db, gleam_toml, _)
  use gen <- result.try(reduce_components(function.parameters, mapper))
  use ret <- result.map(type_to_json(db, gleam_toml, function.return))
  gen
  |> pair.map_second(fn(s) { set.to_list(set.union(s, ret.1)) })
  |> pair.map_first(fn(parameters) {
    json.object([
      #("type", json.string("function")),
      #("name", json.string(function_name)),
      #("documentation", json.nullable(function.documentation, json.string)),
      #("deprecation", json.nullable(function.documentation, json.string)),
      #("implementations", implementations_to_json(function.implementations)),
      #("parameters", json.preprocessed_array(parameters)),
      #("return", ret.0),
    ])
  })
}
