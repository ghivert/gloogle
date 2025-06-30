import gleam/dynamic/decode
import gleam/json

pub type Kind {
  Function
  TypeDefinition
  TypeAlias
  Constant
}

pub fn decoder() {
  use str <- decode.then(decode.string)
  case str {
    "function" -> decode.success(Function)
    "type_definition" -> decode.success(TypeDefinition)
    "type_alias" -> decode.success(TypeAlias)
    "constant" -> decode.success(Constant)
    _ -> decode.failure(Function, "Kind")
  }
}

pub fn encode(kind: Kind) {
  json.string({
    case kind {
      Function -> "function"
      TypeDefinition -> "type_definition"
      TypeAlias -> "type_alias"
      Constant -> "constant"
    }
  })
}

pub fn display(kind) {
  case kind {
    Function -> "Function"
    TypeDefinition -> "Type"
    TypeAlias -> "Type Alias"
    Constant -> "Constant"
  }
}
