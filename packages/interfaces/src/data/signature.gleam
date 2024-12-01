import gleam/dynamic
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
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

pub fn decode(dyn) {
  use res <- result.try(dynamic.field("kind", dynamic.string)(dyn))
  case res {
    "constant" -> decode_constant(dyn)
    "function" -> decode_function(dyn)
    "type-alias" -> decode_type_alias(dyn)
    "type-definition" -> decode_type_definition(dyn)
    _ -> Error([dynamic.DecodeError("", "", [])])
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

fn decode_type(dyn) {
  use res <- result.try(dynamic.field("kind", dynamic.string)(dyn))
  case res {
    "variable" -> decode_variable(dyn)
    "fn" -> decode_fn(dyn)
    "tuple" -> decode_tuple(dyn)
    "named" -> decode_named(dyn)
    _ -> Error([dynamic.DecodeError("", "", [])])
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

fn decode_variable(dyn) {
  dynamic.decode1(fn(a) { Variable(1, a) }, dynamic.field("id", dynamic.int))(
    dyn,
  )
}

fn decode_fn(dyn) {
  dynamic.decode2(
    fn(a, b) {
      let width = {
        [b, ..a]
        |> list.fold(0, fn(acc, val: Type) { val.width + acc })
        |> int.add({ { int.max(list.length(a) - 1, 0) } * 2 } + 8)
      }
      Fn(width, a, b)
    },
    dynamic.field("params", dynamic.list(decode_type)),
    dynamic.field("return", decode_type),
  )(dyn)
}

fn decode_tuple(dyn) {
  dynamic.decode1(
    fn(a) {
      let width =
        list.fold(a, 0, fn(acc, val: Type) { val.width + acc })
        |> int.add({ { int.max(list.length(a) - 1, 0) } * 2 } + 3)
      Tuple(width, a)
    },
    dynamic.field("elements", dynamic.list(decode_type)),
  )(dyn)
}

fn decode_named(dyn) {
  dynamic.decode5(
    fn(a, b, c, d, e) {
      let params_width = list.fold(d, 0, fn(acc, val: Type) { val.width + acc })
      let width =
        string.length(a)
        + case params_width {
          0 -> 0
          value -> value + { { int.max(list.length(d) - 1, 0) } * 2 } + 8
        }
      Named(width, a, b, c, d, e)
    },
    dynamic.field("name", dynamic.string),
    dynamic.field("package", dynamic.string),
    dynamic.field("module", dynamic.string),
    dynamic.field("parameters", dynamic.list(decode_type)),
    dynamic.field("ref", dynamic.optional(dynamic.string)),
  )(dyn)
}

fn decode_parameter(dyn) {
  dynamic.decode2(
    fn(a, b: Type) {
      let width =
        case string.length(option.unwrap(a, "")) {
          0 -> 0
          value -> value + 2
        }
        + b.width
      Parameter(width, a, b)
    },
    dynamic.field("label", dynamic.optional(dynamic.string)),
    dynamic.field("type", decode_type),
  )(dyn)
}

pub fn encode_parameter(parameter: Parameter) {
  json.object([
    #("label", json.nullable(parameter.label, json.string)),
    #("type", encode_type(parameter.type_)),
  ])
}

fn decode_constant(dyn) {
  dynamic.decode1(
    fn(a: Type) {
      let width = a.width
      Constant(width, a)
    },
    dynamic.field("type", decode_type),
  )(dyn)
}

fn decode_function(dyn) {
  dynamic.decode3(
    fn(a: String, b: Type, c) {
      let params_width =
        c
        |> list.fold(0, fn(acc, val: Parameter) { val.width + acc })
        |> int.add({ { int.max(list.length(c) - 1, 0) } * 2 } + 2)
      let width =
        params_width
        |> int.add(b.width + string.length(a) + 6)
      Function(width, params_width, a, b, c)
    },
    dynamic.field("name", dynamic.string),
    dynamic.field("return", decode_type),
    dynamic.field("parameters", dynamic.list(decode_parameter)),
  )(dyn)
}

fn decode_type_alias(dyn) {
  dynamic.decode2(
    fn(a, b: Type) {
      let width = { a * 2 } + 2 + b.width + 3
      TypeAlias(width, a, b)
    },
    dynamic.field("parameters", dynamic.int),
    dynamic.field("alias", decode_type),
  )(dyn)
}

fn decode_type_definition(dyn) {
  dynamic.decode2(
    TypeDefinition,
    dynamic.field("parameters", dynamic.int),
    dynamic.field("constructors", dynamic.list(decode_constructors)),
  )(dyn)
}

fn decode_constructors(dyn) {
  dynamic.decode3(
    fn(a, b, c) {
      let params_width =
        c
        |> list.fold(0, fn(acc, val: Parameter) { val.width + acc })
        |> int.add({ { int.max(list.length(c) - 1, 0) } * 2 } + 2)
      let width =
        params_width
        |> int.add(string.length(b) + 1)
      TypeConstructor(width, params_width, a, b, c)
    },
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("name", dynamic.string),
    dynamic.field("parameters", dynamic.list(decode_parameter)),
  )(dyn)
}

fn encode_constructors(constructor: TypeConstructor) {
  json.object([
    #("documentation", json.nullable(constructor.documentation, json.string)),
    #("name", json.string(constructor.name)),
    #("parameters", json.array(constructor.parameters, encode_parameter)),
  ])
}
