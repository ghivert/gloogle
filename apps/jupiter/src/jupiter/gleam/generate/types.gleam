import gleam/json.{type Json}
import gleam/list
import gleam/package_interface as pi
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import jupiter/loss.{type Loss}

fn reduce_components(
  components: List(a),
  mapper: fn(a) -> Loss(#(Json, Set(String))),
) -> Loss(#(List(Json), Set(String))) {
  let init = Ok(#([], set.new()))
  use acc, val <- list.fold_right(components, init)
  use #(constructors, old_ids) <- result.try(acc)
  use gen <- result.map(mapper(val))
  let #(constructor, new_ids) = gen
  #([constructor, ..constructors], set.union(of: new_ids, and: old_ids))
}

pub fn type_definition_to_json(
  type_name: String,
  type_def: pi.TypeDefinition,
) -> Loss(#(Json, List(String))) {
  use gen <- result.map({
    reduce_components(type_def.constructors, type_constructor_to_json)
  })
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

fn type_constructor_to_json(constructor: pi.TypeConstructor) {
  use gen <- result.map({
    reduce_components(constructor.parameters, parameters_to_json)
  })
  use parameters <- pair.map_first(gen)
  json.object([
    #("kind", json.string("type-constructor")),
    #("documentation", json.nullable(constructor.documentation, json.string)),
    #("name", json.string(constructor.name)),
    #("parameters", json.preprocessed_array(parameters)),
  ])
}

fn parameters_to_json(parameter: pi.Parameter) {
  use gen <- result.map(type_to_json(parameter.type_))
  use type_ <- pair.map_first(gen)
  json.object([
    #("kind", json.string("parameter")),
    #("label", json.nullable(parameter.label, json.string)),
    #("type", type_),
  ])
}

fn type_to_json(type_: pi.Type) {
  case type_ {
    pi.Tuple(elements) -> {
      use gen <- result.map(reduce_components(elements, type_to_json))
      use elements <- pair.map_first(gen)
      json.object([
        #("kind", json.string("tuple")),
        #("elements", json.preprocessed_array(elements)),
      ])
    }
    pi.Fn(params, return) -> {
      use #(elements, params) <- result.try({
        reduce_components(params, type_to_json)
      })
      use gen <- result.map(type_to_json(return))
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
      use gen <- result.map(reduce_components(parameters, type_to_json))
      json.object([
        #("kind", json.string("named")),
        #("name", json.string(name)),
        #("package", json.string(package)),
        #("module", json.string(module)),
        #("parameters", json.preprocessed_array(gen.0)),
      ])
      |> pair.new(gen.1)
    }
  }
}

pub fn type_alias_to_json(
  type_name: String,
  type_alias: pi.TypeAlias,
) -> Loss(#(Json, List(String))) {
  use gen <- result.map(type_to_json(type_alias.alias))
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
  constant_name: String,
  constant: pi.Constant,
) -> Loss(#(Json, List(String))) {
  use gen <- result.map(type_to_json(constant.type_))
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
  function_name: String,
  function: pi.Function,
) -> Loss(#(Json, List(String))) {
  use gen <- result.try({
    reduce_components(function.parameters, parameters_to_json)
  })
  use #(return, parameters) <- result.map(type_to_json(function.return))
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
