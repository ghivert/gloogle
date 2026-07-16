//// Renders a `package_interface` declaration into the stored `signature_json`
//// (the faithful, target-language shape used for display). Pure `pi → Json` —
//// it cannot fail (no DB, no parsing), so no `Loss`. Cross-references are no
//// longer resolved at ingestion; they resolve at query time from the emitted
//// `name`/`package`/`module` coordinates.

import gleam/json.{type Json}
import gleam/package_interface as pi

pub fn type_definition_to_json(
  type_name: String,
  type_def: pi.TypeDefinition,
) -> Json {
  json.object([
    #("kind", json.string("type-definition")),
    #("name", json.string(type_name)),
    #("documentation", json.nullable(type_def.documentation, json.string)),
    #("deprecation", json.nullable(type_def.documentation, json.string)),
    #("parameters", json.int(type_def.parameters)),
    #(
      "constructors",
      json.array(type_def.constructors, type_constructor_to_json),
    ),
  ])
}

fn type_constructor_to_json(constructor: pi.TypeConstructor) -> Json {
  json.object([
    #("kind", json.string("type-constructor")),
    #("documentation", json.nullable(constructor.documentation, json.string)),
    #("name", json.string(constructor.name)),
    #("parameters", json.array(constructor.parameters, parameters_to_json)),
  ])
}

fn parameters_to_json(parameter: pi.Parameter) -> Json {
  json.object([
    #("kind", json.string("parameter")),
    #("label", json.nullable(parameter.label, json.string)),
    #("type", type_to_json(parameter.type_)),
  ])
}

fn type_to_json(type_: pi.Type) -> Json {
  case type_ {
    pi.Tuple(elements) ->
      json.object([
        #("kind", json.string("tuple")),
        #("elements", json.array(elements, type_to_json)),
      ])
    pi.Fn(params, return) ->
      json.object([
        #("kind", json.string("fn")),
        #("params", json.array(params, type_to_json)),
        #("return", type_to_json(return)),
      ])
    pi.Variable(id) ->
      json.object([
        #("kind", json.string("variable")),
        #("id", json.int(id)),
      ])
    pi.Named(name, package, module, parameters) ->
      json.object([
        #("kind", json.string("named")),
        #("name", json.string(name)),
        #("package", json.string(package)),
        #("module", json.string(module)),
        #("parameters", json.array(parameters, type_to_json)),
      ])
  }
}

pub fn type_alias_to_json(type_name: String, type_alias: pi.TypeAlias) -> Json {
  json.object([
    #("kind", json.string("type-alias")),
    #("name", json.string(type_name)),
    #("documentation", json.nullable(type_alias.documentation, json.string)),
    #("deprecation", json.nullable(type_alias.documentation, json.string)),
    #("parameters", json.int(type_alias.parameters)),
    #("alias", type_to_json(type_alias.alias)),
  ])
}

pub fn implementations_to_json(implementations: pi.Implementations) -> Json {
  let uses_js = json.bool(implementations.uses_javascript_externals)
  json.object([
    #("gleam", json.bool(implementations.gleam)),
    #("uses_erlang_externals", json.bool(implementations.uses_erlang_externals)),
    #("uses_javascript_externals", uses_js),
  ])
}

pub fn constant_to_json(constant_name: String, constant: pi.Constant) -> Json {
  json.object([
    #("kind", json.string("constant")),
    #("name", json.string(constant_name)),
    #("documentation", json.nullable(constant.documentation, json.string)),
    #("deprecation", json.nullable(constant.documentation, json.string)),
    #("implementations", implementations_to_json(constant.implementations)),
    #("type", type_to_json(constant.type_)),
  ])
}

pub fn function_to_json(function_name: String, function: pi.Function) -> Json {
  json.object([
    #("kind", json.string("function")),
    #("name", json.string(function_name)),
    #("documentation", json.nullable(function.documentation, json.string)),
    #("deprecation", json.nullable(function.documentation, json.string)),
    #("implementations", implementations_to_json(function.implementations)),
    #("parameters", json.array(function.parameters, parameters_to_json)),
    #("return", type_to_json(function.return)),
  ])
}
