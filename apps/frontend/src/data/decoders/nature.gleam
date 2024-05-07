import gleam/dynamic
import gleam/result

pub type Nature {
  Function
  TypeDefinition
  TypeAlias
  Constant
}

pub fn decode_nature(dyn) {
  use str <- result.try(dynamic.string(dyn))
  case str {
    "function" -> Ok(Function)
    "type_definition" -> Ok(TypeDefinition)
    "type_alias" -> Ok(TypeAlias)
    "constant" -> Ok(Constant)
    _ -> Error([dynamic.DecodeError("", "", [])])
  }
}

pub fn display_nature(nature) {
  case nature {
    Function -> "Function"
    TypeDefinition -> "Type"
    TypeAlias -> "Type Alias"
    Constant -> "Constant"
  }
}
