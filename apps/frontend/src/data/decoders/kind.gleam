import gleam/dynamic
import gleam/result

pub type Kind {
  Function
  TypeDefinition
  TypeAlias
  Constant
}

pub fn decode_kind(dyn) {
  use str <- result.try(dynamic.string(dyn))
  case str {
    "function" -> Ok(Function)
    "type_definition" -> Ok(TypeDefinition)
    "type_alias" -> Ok(TypeAlias)
    "constant" -> Ok(Constant)
    _ -> Error([dynamic.DecodeError("", "", [])])
  }
}

pub fn display_kind(kind) {
  case kind {
    Function -> "Function"
    TypeDefinition -> "Type"
    TypeAlias -> "Type Alias"
    Constant -> "Constant"
  }
}
