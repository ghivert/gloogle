import gleam/bit_array
import gleam/bool
import gleam/dict
import gleam/dynamic/decode
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order.{type Order}
import gleam/package_interface as pi
import gleam/pair
import gleam/result
import gleam/semver
import gleam/set.{type Set}
import gleam/string
import jupiter/error
import jupiter/gleam/context.{type Context}
import jupiter/gleam/toml
import pog

fn reduce_components(
  components: List(a),
  mapper: fn(a) -> Result(#(Json, Set(Int)), error.Error),
) -> Result(#(List(Json), Set(Int)), error.Error) {
  let init = Ok(#([], set.new()))
  use acc, val <- list.fold_right(components, init)
  use #(constructors, old_ids) <- result.try(acc)
  use gen <- result.map(mapper(val))
  let #(constructor, new_ids) = gen
  #([constructor, ..constructors], set.union(of: new_ids, and: old_ids))
}

pub fn type_definition_to_json(
  ctx: Context,
  type_name: String,
  type_def: pi.TypeDefinition,
) -> Result(#(Json, List(Int)), error.Error) {
  let mapper = type_constructor_to_json(ctx, _)
  use gen <- result.map(reduce_components(type_def.constructors, mapper))
  use constructors <- pair.map_first(pair.map_second(gen, set.to_list))
  json.object([
    #("kind", json.string("type-definition")),
    #("name", json.string(type_name)),
    #("documentation", json.nullable(type_def.documentation, json.string)),
    #("deprecation", json.nullable(type_def.documentation, json.string)),
    #("parameters", json.int(type_def.parameters)),
    #("constructors", json.preprocessed_array(constructors)),
  ])
}

fn type_constructor_to_json(ctx: Context, constructor: pi.TypeConstructor) {
  let mapper = parameters_to_json(ctx, _)
  use gen <- result.map(reduce_components(constructor.parameters, mapper))
  use parameters <- pair.map_first(gen)
  json.object([
    #("kind", json.string("type-constructor")),
    #("documentation", json.nullable(constructor.documentation, json.string)),
    #("name", json.string(constructor.name)),
    #("parameters", json.preprocessed_array(parameters)),
  ])
}

fn parameters_to_json(ctx: Context, parameter: pi.Parameter) {
  use gen <- result.map(type_to_json(ctx, parameter.type_))
  use type_ <- pair.map_first(gen)
  json.object([
    #("kind", json.string("parameter")),
    #("label", json.nullable(parameter.label, json.string)),
    #("type", type_),
  ])
}

fn type_to_json(ctx: Context, type_: pi.Type) {
  case type_ {
    pi.Tuple(elements) -> {
      let mapper = type_to_json(ctx, _)
      use gen <- result.map(reduce_components(elements, mapper))
      use elements <- pair.map_first(gen)
      json.object([
        #("kind", json.string("tuple")),
        #("elements", json.preprocessed_array(elements)),
      ])
    }
    pi.Fn(params, return) -> {
      let mapper = type_to_json(ctx, _)
      use #(elements, params) <- result.try(reduce_components(params, mapper))
      use gen <- result.map(type_to_json(ctx, return))
      let new_params = set.union(of: params, and: gen.1)
      json.object([
        #("kind", json.string("fn")),
        #("params", json.preprocessed_array(elements)),
        #("return", gen.0),
      ])
      |> pair.new(new_params)
    }
    pi.Variable(id) -> {
      let json =
        json.object([
          #("kind", json.string("variable")),
          #("id", json.int(id)),
        ])
      Ok(#(json, set.new()))
    }
    pi.Named(name, package, module, parameters) -> {
      let mapper = type_to_json(ctx, _)
      use gen <- result.try(reduce_components(parameters, mapper))
      let res = extract_parameters_relation(ctx, name, package, module)
      use ref <- result.map(res)
      let new_ids = case ref {
        None -> gen.1
        Some(ref) -> set.insert(gen.1, ref.1)
      }
      json.object([
        #("kind", json.string("named")),
        #("ref", json.nullable(option.map(ref, fn(r) { r.0 }), json.string)),
        #("name", json.string(name)),
        #("package", json.string(package)),
        #("module", json.string(module)),
        #("parameters", json.preprocessed_array(gen.0)),
      ])
      |> pair.new(new_ids)
    }
  }
}

fn find_package_release(ctx: Context, package: String, requirement: String) {
  "SELECT package_release.id id, package_release.version version
  FROM package
  JOIN package_release
    ON package.id = package_release.package_id
  WHERE package.name = $1"
  |> pog.query
  |> pog.parameter(pog.text(package))
  |> pog.returning({
    use id <- decode.field("id", decode.int)
    use version <- decode.field("version", decode.string)
    decode.success(#(id, version))
  })
  |> pog.execute(ctx.db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(response) { response.rows })
  |> result.map(keep_matching_releases(_, requirement))
}

fn keep_matching_releases(rows: List(#(Int, String)), requirement: String) {
  let by_matching_requirement = keep_matching_requirement(_, requirement)
  rows
  |> list.filter(by_matching_requirement)
  |> list.sort(by_decreasing_version)
  |> list.map(pair.first)
}

fn keep_matching_requirement(release: #(Int, String), requirement: String) {
  let #(_release_id, release_version) = release
  let requirement = bit_array.from_string(requirement)
  let version = bit_array.from_string(release_version)
  let is_matching = semver.is_match(version:, requirement:)
  result.unwrap(is_matching, False)
}

fn by_decreasing_version(
  release_1: #(Int, String),
  release_2: #(Int, String),
) -> Order {
  let #(_, release_1_version) = release_1
  let #(_, release_2_version) = release_2
  let release_1_version = bit_array.from_string(release_1_version)
  let release_2_version = bit_array.from_string(release_2_version)
  case semver.gte(version: release_1_version, with: release_2_version) {
    True -> order.Lt
    False -> order.Gt
  }
}

fn find_signature_from_release(
  ctx: Context,
  name: String,
  module: String,
  releases: List(Int),
) -> Result(#(String, Int), error.Error) {
  use acc, release <- list.fold(releases, error.empty())
  use <- bool.guard(when: result.is_ok(acc), return: acc)
  "SELECT release.version version, signature.id id
  FROM package_release release
  JOIN package_module module
    ON module.package_release_id = release.id
  JOIN package_type_fun_signature signature
    ON signature.package_module_id = module.id
  WHERE signature.name = $1
    AND module.name = $2
    AND module.package_release_id = $3"
  |> pog.query
  |> pog.parameter(pog.text(name))
  |> pog.parameter(pog.text(module))
  |> pog.parameter(pog.int(release))
  |> pog.returning({
    use version <- decode.field("version", decode.string)
    use id <- decode.field("id", decode.int)
    decode.success(#(version, id))
  })
  |> pog.execute(ctx.db)
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(response) {
    let error = error.CustomError("[find_signature_from_release] No row")
    list.first(response.rows)
    |> result.replace_error(error)
  })
}

fn find_type_signature(
  ctx: Context,
  name: String,
  package: String,
  module: String,
  releases: List(Int),
) -> Result(Option(#(String, Int)), error.Error) {
  find_signature_from_release(ctx, name, module, releases)
  |> result.map(Some)
  |> result.lazy_or(fn() {
    let slug = string.join([package, module], with: "/")
    let package_name = ctx.package_interface.name
    case ctx.package_interface.name == package {
      // Not the same package, coming from an external package, should wait
      // for it to be extracted. It's impossible to get a type hidden by the
      // package, should it should work in the long run.
      False -> {
        { package_name <> ", needs to access " <> slug }
        |> fn(content) { "Inside " <> content <> ". Not found." }
        |> error.CustomError
        |> Error
      }
      True -> {
        case dict.get(ctx.package_interface.modules, module) {
          // Module is hidden, everything is correct, type is hidden.
          Error(_) -> Ok(None)
          // Module is not hidden, checking if type is hidden by itself.
          Ok(mod) -> {
            let slug = string.join([slug, name], with: ".")
            case dict.get(mod.type_aliases, name) {
              // Type is not hidden, returning an error to restart the extraction.
              Ok(_) -> {
                { package_name <> ", looking for " <> slug }
                |> fn(id) { "Inside type aliases " <> id <> ". Not found." }
                |> error.CustomError
                |> Error
              }
              // Type is hidden, should check if type defed.
              Error(_) -> {
                case dict.get(mod.types, name) {
                  // Type is hidden, returning None because it can't be extracted.
                  Error(_) -> Ok(None)
                  // Type is not hidden, returning an error to restart the extraction.
                  Ok(_) -> {
                    { package_name <> ", looking for " <> slug }
                    |> fn(id) { "Inside types " <> id <> ". Not found." }
                    |> error.CustomError
                    |> Error
                  }
                }
              }
            }
          }
        }
      }
    }
  })
}

fn extract_parameters_relation(
  ctx: Context,
  name: String,
  package: String,
  module: String,
) -> Result(Option(#(String, Int)), error.Error) {
  use <- bool.guard(when: is_prelude(package, module), return: Ok(None))
  use requirement <- result.try(toml.find_package_requirement(ctx, package))
  use releases <- result.try(find_package_release(ctx, package, requirement))
  find_type_signature(ctx, name, package, module, releases)
  |> result.try_recover(fn(error) {
    use <- bool.guard(when: ctx.ignore_parameters_errors, return: Ok(None))
    Error(error)
  })
}

fn is_prelude(package: String, module: String) {
  module == "gleam" && package == ""
}

pub fn type_alias_to_json(
  ctx: Context,
  type_name: String,
  type_alias: pi.TypeAlias,
) {
  use gen <- result.map(type_to_json(ctx, type_alias.alias))
  use alias <- pair.map_first(pair.map_second(gen, set.to_list))
  json.object([
    #("kind", json.string("type-alias")),
    #("name", json.string(type_name)),
    #("documentation", json.nullable(type_alias.documentation, json.string)),
    #("deprecation", json.nullable(type_alias.documentation, json.string)),
    #("parameters", json.int(type_alias.parameters)),
    #("alias", alias),
  ])
}

pub fn implementations_to_json(implementations: pi.Implementations) {
  let uses_js = json.bool(implementations.uses_javascript_externals)
  json.object([
    #("gleam", json.bool(implementations.gleam)),
    #("uses_erlang_externals", json.bool(implementations.uses_erlang_externals)),
    #("uses_javascript_externals", uses_js),
  ])
}

pub fn constant_to_json(
  ctx: Context,
  constant_name: String,
  constant: pi.Constant,
) -> Result(#(Json, List(Int)), error.Error) {
  use gen <- result.map(type_to_json(ctx, constant.type_))
  use type_ <- pair.map_first(pair.map_second(gen, set.to_list))
  json.object([
    #("kind", json.string("constant")),
    #("name", json.string(constant_name)),
    #("documentation", json.nullable(constant.documentation, json.string)),
    #("deprecation", json.nullable(constant.documentation, json.string)),
    #("implementations", implementations_to_json(constant.implementations)),
    #("type", type_),
  ])
}

pub fn function_to_json(
  ctx: Context,
  function_name: String,
  function: pi.Function,
) -> Result(#(Json, List(Int)), error.Error) {
  let mapper = parameters_to_json(ctx, _)
  use gen <- result.try(reduce_components(function.parameters, mapper))
  use #(return, parameters) <- result.map(type_to_json(ctx, function.return))
  gen
  |> pair.map_second(fn(s) { set.to_list(set.union(s, parameters)) })
  |> pair.map_first(fn(parameters) {
    json.object([
      #("kind", json.string("function")),
      #("name", json.string(function_name)),
      #("documentation", json.nullable(function.documentation, json.string)),
      #("deprecation", json.nullable(function.documentation, json.string)),
      #("implementations", implementations_to_json(function.implementations)),
      #("parameters", json.preprocessed_array(parameters)),
      #("return", return),
    ])
  })
}
