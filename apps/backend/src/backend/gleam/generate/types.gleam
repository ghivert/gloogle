import backend/error
import backend/gleam/context.{type Context}
import backend/gleam/toml
import gleam/bit_array
import gleam/bool
import gleam/dict
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
  ctx: Context,
  type_name: String,
  type_def: TypeDefinition,
) -> Result(#(Json, List(Int)), error.Error) {
  let mapper = type_constructor_to_json(ctx, _)
  use gen <- result.map(reduce_components(type_def.constructors, mapper))
  use constructors <- pair.map_first(pair.map_second(gen, set.to_list))
  json.object([
    #("nature", json.string("type-definition")),
    #("name", json.string(type_name)),
    #("documentation", json.nullable(type_def.documentation, json.string)),
    #("deprecation", json.nullable(type_def.documentation, json.string)),
    #("parameters", json.int(type_def.parameters)),
    #("constructors", json.preprocessed_array(constructors)),
  ])
}

fn type_constructor_to_json(ctx: Context, constructor: TypeConstructor) {
  let mapper = parameters_to_json(ctx, _)
  use gen <- result.map(reduce_components(constructor.parameters, mapper))
  use parameters <- pair.map_first(gen)
  json.object([
    #("type", json.string("type-constructor")),
    #("documentation", json.nullable(constructor.documentation, json.string)),
    #("name", json.string(constructor.name)),
    #("parameters", json.preprocessed_array(parameters)),
  ])
}

fn parameters_to_json(ctx: Context, parameter: Parameter) {
  use gen <- result.map(type_to_json(ctx, parameter.type_))
  use type_ <- pair.map_first(gen)
  json.object([
    #("type", json.string("parameter")),
    #("label", json.nullable(parameter.label, json.string)),
    #("params_type", type_),
  ])
}

fn type_to_json(ctx: Context, type_: Type) {
  case type_ {
    package_interface.Tuple(elements) -> {
      let mapper = type_to_json(ctx, _)
      use gen <- result.map(reduce_components(elements, mapper))
      use elements <- pair.map_first(gen)
      json.object([
        #("type", json.string("tuple")),
        #("elements", json.preprocessed_array(elements)),
      ])
    }
    package_interface.Fn(params, return) -> {
      let mapper = type_to_json(ctx, _)
      use #(elements, params) <- result.try(reduce_components(params, mapper))
      use gen <- result.map(type_to_json(ctx, return))
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
      let mapper = type_to_json(ctx, _)
      use gen <- result.try(reduce_components(parameters, mapper))
      let res = extract_parameters_relation(ctx, name, package, module)
      use ref <- result.map(res)
      let new_ids = case ref {
        option.None -> gen.1
        option.Some(ref) -> set.insert(gen.1, ref.1)
      }
      json.object([
        #("type", json.string("named")),
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
  let decoder = dynamic.tuple2(dynamic.int, dynamic.string)
  use response <- result.try({
    "SELECT package_release.id, package_release.version
     FROM package
     JOIN package_release
       ON package.id = package_release.package_id
     WHERE package.name = $1"
    |> pgo.execute(ctx.db, [pgo.text(package)], decoder)
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

fn find_signature_from_release(
  ctx: Context,
  name: String,
  module: String,
  releases: List(Int),
) {
  use acc, release <- list.fold(releases, Error(Nil))
  use <- bool.guard(when: result.is_ok(acc), return: acc)
  let args = [pgo.text(name), pgo.text(module), pgo.int(release)]
  use t <- result.try({
    "SELECT release.version, signature.id
     FROM package_release release
     JOIN package_module module
       ON module.package_release_id = release.id
     JOIN package_type_fun_signature signature
       ON signature.package_module_id = module.id
     WHERE signature.name = $1
       AND module.name = $2
       AND module.package_release_id = $3"
    |> pgo.execute(ctx.db, args, dynamic.tuple2(dynamic.string, dynamic.int))
    |> result.nil_error()
  })
  list.first(t.rows)
  |> result.nil_error()
}

fn find_type_signature(
  ctx: Context,
  name: String,
  package: String,
  module: String,
  releases: List(Int),
) -> Result(option.Option(#(String, Int)), error.Error) {
  case find_signature_from_release(ctx, name, module, releases) {
    Ok(value) -> Ok(option.Some(value))
    Error(_) -> {
      let slug = package <> "/" <> module
      let package_name = ctx.package_interface.name
      case ctx.package_interface.name == package {
        // Not the same package, coming from an external package, should wait
        // for it to be extracted. It's impossible to get a type hidden by the
        // package, should it should work in the long run.
        False -> {
          let content = package_name <> ", needs to access " <> slug
          Error(error.UnknownError("Inside " <> content <> ". Not found."))
        }
        True ->
          case dict.get(ctx.package_interface.modules, module) {
            // Module is hidden, everything is correct, type is hidden.
            Error(_) -> Ok(option.None)
            // Module is not hidden, checking if type is hidden by itself.
            Ok(mod) -> {
              let slug = slug <> "." <> name
              case dict.get(mod.type_aliases, name) {
                // Type is not hidden, returning an error to restart the extraction.
                Ok(_) -> {
                  let id = package_name <> ", looking for " <> slug
                  let msg = "Inside type aliases " <> id <> ". Not found."
                  Error(error.UnknownError(msg))
                }
                // Type is hidden, should check if type defed.
                Error(_) ->
                  case dict.get(mod.types, name) {
                    // Type is hidden, returning None because it can't be extracted.
                    Error(_) -> Ok(option.None)
                    // Type is not hidden, returning an error to restart the extraction.
                    Ok(_) -> {
                      let id = package_name <> ", looking for " <> slug
                      let msg = "Inside types " <> id <> ". Not found."
                      Error(error.UnknownError(msg))
                    }
                  }
              }
            }
          }
      }
    }
  }
}

fn extract_parameters_relation(
  ctx: Context,
  name: String,
  package: String,
  module: String,
) -> Result(option.Option(#(String, Int)), error.Error) {
  use <- bool.guard(when: is_prelude(package, module), return: Ok(option.None))
  use requirement <- result.try(toml.find_package_requirement(ctx, package))
  use releases <- result.try(find_package_release(ctx, package, requirement))
  use error <- result.try_recover({
    find_type_signature(ctx, name, package, module, releases)
  })
  case ctx.ignore_parameters_errors {
    False -> Error(error)
    True -> Ok(option.None)
  }
}

fn is_prelude(package: String, module: String) {
  module == "gleam" && package == ""
}

pub fn type_alias_to_json(
  ctx: Context,
  type_name: String,
  type_alias: TypeAlias,
) {
  use gen <- result.map(type_to_json(ctx, type_alias.alias))
  use alias <- pair.map_first(pair.map_second(gen, set.to_list))
  json.object([
    #("nature", json.string("type-alias")),
    #("name", json.string(type_name)),
    #("documentation", json.nullable(type_alias.documentation, json.string)),
    #("deprecation", json.nullable(type_alias.documentation, json.string)),
    #("parameters", json.int(type_alias.parameters)),
    #("alias", alias),
  ])
}

pub fn implementations_to_json(implementations: Implementations) {
  let uses_js = json.bool(implementations.uses_javascript_externals)
  json.object([
    #("gleam", json.bool(implementations.gleam)),
    #("uses_erlang_externals", json.bool(implementations.uses_erlang_externals)),
    #("uses_javascript_externals", uses_js),
  ])
}

pub fn constant_to_json(ctx: Context, constant_name: String, constant: Constant) {
  use gen <- result.map(type_to_json(ctx, constant.type_))
  use type_ <- pair.map_first(pair.map_second(gen, set.to_list))
  json.object([
    #("nature", json.string("constant")),
    #("name", json.string(constant_name)),
    #("documentation", json.nullable(constant.documentation, json.string)),
    #("deprecation", json.nullable(constant.documentation, json.string)),
    #("implementations", implementations_to_json(constant.implementations)),
    #("type", type_),
  ])
}

pub fn function_to_json(ctx: Context, function_name: String, function: Function) {
  let mapper = parameters_to_json(ctx, _)
  use gen <- result.try(reduce_components(function.parameters, mapper))
  use ret <- result.map(type_to_json(ctx, function.return))
  gen
  |> pair.map_second(fn(s) { set.to_list(set.union(s, ret.1)) })
  |> pair.map_first(fn(parameters) {
    json.object([
      #("nature", json.string("function")),
      #("name", json.string(function_name)),
      #("documentation", json.nullable(function.documentation, json.string)),
      #("deprecation", json.nullable(function.documentation, json.string)),
      #("implementations", implementations_to_json(function.implementations)),
      #("parameters", json.preprocessed_array(parameters)),
      #("return", ret.0),
    ])
  })
}
