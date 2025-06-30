import data/kind.{type Kind}
import data/metadata.{type Metadata}
import data/signature.{type Signature}
import decrypt
import gleam/dynamic/decode
import gleam/json

pub type TypeSearch {
  TypeSearch(
    type_name: String,
    documentation: String,
    signature_kind: Kind,
    metadata: Metadata,
    json_signature: Signature,
    module_name: String,
    package_name: String,
    version: String,
  )
}

pub fn decoder() {
  use type_name <- decode.field("type_name", decode.string)
  use documentation <- decode.field("documentation", decode.string)
  use signature_kind <- decode.field("signature_kind", kind.decoder())
  use metadata <- decode.field("metadata", decrypt.json(metadata.decoder()))
  let json_signature = decrypt.json(signature.decoder())
  use json_signature <- decode.field("json_signature", json_signature)
  use module_name <- decode.field("module_name", decode.string)
  use package_name <- decode.field("package_name", decode.string)
  use version <- decode.field("version", decode.string)
  decode.success({
    TypeSearch(
      type_name:,
      documentation:,
      signature_kind:,
      metadata:,
      json_signature:,
      module_name:,
      package_name:,
      version:,
    )
  })
}

pub fn encode(item: TypeSearch) {
  json.object([
    #("type_name", json.string(item.type_name)),
    #("documentation", json.string(item.documentation)),
    #("signature_kind", kind.encode(item.signature_kind)),
    #("metadata", metadata.encode(item.metadata)),
    #("json_signature", signature.encode(item.json_signature)),
    #("module_name", json.string(item.module_name)),
    #("package_name", json.string(item.package_name)),
    #("version", json.string(item.version)),
  ])
}
