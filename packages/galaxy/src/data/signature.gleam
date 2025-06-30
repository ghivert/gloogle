import decrypt
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/string

pub type Type {
  Tuple(width: Int, elements: List(Type))
  Fn(width: Int, parameters: List(Type), return: Type)
  Variable(width: Int, id: Int)
  Named(
    width: Int,
    name: String,
    package: String,
    module: String,
    parameters: List(Type),
    ref: Option(String),
  )
}

pub type Parameter {
  Parameter(width: Int, label: Option(String), type_: Type)
}

pub type TypeConstructor {
  TypeConstructor(
    width: Int,
    params_width: Int,
    documentation: Option(String),
    name: String,
    parameters: List(Parameter),
  )
}

pub type Signature {
  Function(
    width: Int,
    params_width: Int,
    name: String,
    return: Type,
    parameters: List(Parameter),
  )
  Constant(width: Int, type_: Type)
  TypeAlias(width: Int, parameters: Int, alias: Type)
  TypeDefinition(parameters: Int, constructors: List(TypeConstructor))
}

pub fn decoder() {
  use res <- decode.field("kind", decode.string)
  case res {
    "constant" -> constant_decoder()
    "function" -> function_decoder()
    "type-alias" -> type_alias_decoder()
    "type-definition" -> type_definition_decoder()
    _ -> decode.failure(TypeDefinition(0, []), "Kind")
  }
}

pub fn encode(signature: Signature) {
  case signature {
    Constant(type_:, ..) ->
      json.object([
        #("kind", json.string("constant")),
        #("type", encode_type(type_)),
      ])
    TypeAlias(parameters:, alias:, ..) ->
      json.object([
        #("kind", json.string("type-alias")),
        #("parameters", json.int(parameters)),
        #("alias", encode_type(alias)),
      ])
    TypeDefinition(parameters:, constructors:) ->
      json.object([
        #("kind", json.string("type-definition")),
        #("parameters", json.int(parameters)),
        #("constructors", json.array(constructors, encode_constructors)),
      ])
    Function(name:, return:, parameters:, ..) ->
      json.object([
        #("kind", json.string("function")),
        #("name", json.string(name)),
        #("return", encode_type(return)),
        #("parameters", json.array(parameters, encode_parameter)),
      ])
  }
}

fn type_decoder() {
  use res <- decode.field("kind", decode.string)
  case res {
    "variable" -> variable_decoder()
    "fn" -> fn_decoder()
    "tuple" -> tuple_decoder()
    "named" -> named_decoder()
    _ -> decode.failure(Variable(0, 0), "Type")
  }
}

fn encode_type(type_: Type) {
  case type_ {
    Variable(id:, ..) ->
      json.object([#("kind", json.string("variable")), #("id", json.int(id))])
    Fn(parameters:, return:, ..) ->
      json.object([
        #("kind", json.string("fn")),
        #("params", json.array(parameters, encode_type)),
        #("return", encode_type(return)),
      ])
    Named(name:, package:, module:, parameters:, ref:, ..) ->
      json.object([
        #("kind", json.string("named")),
        #("name", json.string(name)),
        #("package", json.string(package)),
        #("module", json.string(module)),
        #("parameters", json.array(parameters, encode_type)),
        #("ref", json.nullable(ref, json.string)),
      ])
    Tuple(elements:, ..) ->
      json.object([
        #("kind", json.string("tuple")),
        #("elements", json.array(elements, encode_type)),
      ])
  }
}

fn variable_decoder() {
  use id <- decode.field("id", decode.int)
  decode.success(Variable(1, id))
}

fn fn_decoder() {
  use params <- decode.field("params", decode.list(type_decoder()))
  use return <- decode.field("return", type_decoder())
  let width = {
    [return, ..params]
    |> list.fold(0, fn(acc, val: Type) { val.width + acc })
    |> int.add({ { int.max(list.length(params) - 1, 0) } * 2 } + 8)
  }
  decode.success(Fn(width, params, return))
}

fn tuple_decoder() {
  use elements <- decode.field("elements", decode.list(type_decoder()))
  let width =
    list.fold(elements, 0, fn(acc, val: Type) { val.width + acc })
    |> int.add({ { int.max(list.length(elements) - 1, 0) } * 2 } + 3)
  decode.success(Tuple(width, elements))
}

fn named_decoder() {
  use name <- decode.field("name", decode.string)
  use package <- decode.field("package", decode.string)
  use module <- decode.field("module", decode.string)
  use parameters <- decode.field("parameters", decode.list(type_decoder()))
  use ref <- decode.field("ref", decode.optional(decode.string))
  let params_width =
    list.fold(parameters, 0, fn(acc, val: Type) { val.width + acc })
  let width =
    string.length(name)
    + case params_width {
      0 -> 0
      value -> value + { { int.max(list.length(parameters) - 1, 0) } * 2 } + 8
    }
  decode.success(Named(width:, name:, package:, module:, parameters:, ref:))
}

fn parameter_decoder() {
  use label <- decode.field("label", decode.optional(decode.string))
  use type_ <- decode.field("type", type_decoder())
  let width =
    case string.length(option.unwrap(label, "")) {
      0 -> 0
      value -> value + 2
    }
    + type_.width
  decode.success(Parameter(width:, label:, type_:))
}

pub fn encode_parameter(parameter: Parameter) {
  json.object([
    #("label", json.nullable(parameter.label, json.string)),
    #("type", encode_type(parameter.type_)),
  ])
}

fn constant_decoder() {
  use type_ <- decode.field("type", type_decoder())
  let width = type_.width
  decode.success(Constant(width:, type_:))
}

fn function_decoder() {
  use name <- decode.field("name", decode.string)
  use return <- decode.field("return", type_decoder())
  use parameters <- decode.field("parameters", decode.list(parameter_decoder()))
  let params_width =
    parameters
    |> list.fold(0, fn(acc, val: Parameter) { val.width + acc })
    |> int.add({ { int.max(list.length(parameters) - 1, 0) } * 2 } + 2)
  let width = int.add(params_width, return.width + string.length(name) + 6)
  decode.success(Function(width, params_width, name, return, parameters))
}

fn type_alias_decoder() {
  use parameters <- decode.field("parameters", decode.int)
  use alias <- decode.field("alias", type_decoder())
  let width = { parameters * 2 } + 2 + alias.width + 3
  decode.success(TypeAlias(width:, parameters:, alias:))
}

fn type_definition_decoder() {
  use parameters <- decode.field("parameters", decode.int)
  use constructors <- decode.field("constructors", {
    decode.list({
      use documentation <- decrypt.optional("documentation", decode.string)
      use name <- decode.field("name", decode.string)
      let parameters = decode.list(parameter_decoder())
      use parameters <- decode.field("parameters", parameters)
      let params_width =
        parameters
        |> list.fold(0, fn(acc, val: Parameter) { val.width + acc })
        |> int.add({ { int.max(list.length(parameters) - 1, 0) } * 2 } + 2)
      let width = int.add(params_width, string.length(name) + 1)
      decode.success({
        TypeConstructor(
          width:,
          params_width:,
          documentation:,
          name:,
          parameters:,
        )
      })
    })
  })
  decode.success(TypeDefinition(parameters:, constructors:))
}

fn encode_constructors(constructor: TypeConstructor) {
  json.object([
    #("documentation", json.nullable(constructor.documentation, json.string)),
    #("name", json.string(constructor.name)),
    #("parameters", json.array(constructor.parameters, encode_parameter)),
  ])
}
