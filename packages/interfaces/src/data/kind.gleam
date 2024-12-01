import gleam/dynamic
import gleam/json
import gleam/result

pub type Kind {
  Function
  TypeDefinition
  TypeAlias
  Constant
}

pub fn decode(dyn) {
  use str <- result.try(dynamic.string(dyn))
  case str {
    "function" -> Ok(Function)
    "type_definition" -> Ok(TypeDefinition)
    "type_alias" -> Ok(TypeAlias)
    "constant" -> Ok(Constant)
    _ -> Error([dynamic.DecodeError("", "", [])])
  }
}

pub fn encode(kind) {
  case kind {
    Function -> "function"
    TypeDefinition -> "type_definition"
    TypeAlias -> "type_alias"
    Constant -> "constant"
  }
  |> json.string
}

pub fn display(kind) {
  case kind {
    Function -> "Function"
    TypeDefinition -> "Type"
    TypeAlias -> "Type Alias"
    Constant -> "Constant"
  }
}
