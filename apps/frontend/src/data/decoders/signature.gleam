import gleam/dynamic
import gleam/option.{type Option}
import gleam/result

pub type Type {
  Tuple(elements: List(Type))
  Fn(parameters: List(Type), return: Type)
  Variable(id: Int)
  Named(
    name: String,
    package: String,
    module: String,
    parameters: List(Type),
    ref: Option(Int),
  )
}

pub type Parameter {
  Parameter(label: Option(String), type_: Type)
}

pub type TypeConstructor {
  TypeConstructor(
    documentation: Option(String),
    name: String,
    parameters: List(Parameter),
  )
}

pub type Signature {
  Function(name: String, return: Type, parameters: List(Parameter))
  Constant(type_: Type)
  TypeAlias(parameters: Int, alias: Type)
  TypeDefinition(parameters: Int, constructors: List(TypeConstructor))
}

pub fn decode_signature(dyn) {
  use res <- result.try(dynamic.field("type", dynamic.string)(dyn))
  case res {
    "constant" -> decode_constant(dyn)
    "function" -> decode_function(dyn)
    "type-alias" -> decode_type_alias(dyn)
    "type-definition" -> decode_type_definition(dyn)
    _ -> Error([dynamic.DecodeError("", "", [])])
  }
}

fn decode_type(dyn) {
  use res <- result.try(dynamic.field("type", dynamic.string)(dyn))
  case res {
    "variable" -> decode_variable(dyn)
    "fn" -> decode_fn(dyn)
    "tuple" -> decode_tuple(dyn)
    "named" -> decode_named(dyn)
    _ -> Error([dynamic.DecodeError("", "", [])])
  }
}

fn decode_variable(dyn) {
  dynamic.decode1(Variable, dynamic.field("id", dynamic.int))(dyn)
}

fn decode_fn(dyn) {
  dynamic.decode2(
    Fn,
    dynamic.field("params", dynamic.list(decode_type)),
    dynamic.field("return", decode_type),
  )(dyn)
}

fn decode_tuple(dyn) {
  dynamic.decode1(Tuple, dynamic.field("elements", dynamic.list(decode_type)))(
    dyn,
  )
}

fn decode_named(dyn) {
  dynamic.decode5(
    Named,
    dynamic.field("name", dynamic.string),
    dynamic.field("package", dynamic.string),
    dynamic.field("module", dynamic.string),
    dynamic.field("parameters", dynamic.list(decode_type)),
    dynamic.field("ref", dynamic.optional(dynamic.int)),
  )(dyn)
}

fn decode_parameter(dyn) {
  dynamic.decode2(
    Parameter,
    dynamic.field("label", dynamic.optional(dynamic.string)),
    dynamic.field("type", decode_type),
  )(dyn)
}

fn decode_constant(dyn) {
  dynamic.decode1(Constant, dynamic.field("type", decode_type))(dyn)
}

fn decode_function(dyn) {
  dynamic.decode3(
    Function,
    dynamic.field("name", dynamic.string),
    dynamic.field("return", decode_type),
    dynamic.field("parameters", dynamic.list(decode_parameter)),
  )(dyn)
}

fn decode_type_alias(dyn) {
  dynamic.decode2(
    TypeAlias,
    dynamic.field("parameters", dynamic.int),
    dynamic.field("type", decode_type),
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
    TypeConstructor,
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("name", dynamic.string),
    dynamic.field("parameters", dynamic.list(decode_parameter)),
  )(dyn)
}
