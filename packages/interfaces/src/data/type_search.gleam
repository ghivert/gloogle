import data/kind.{type Kind}
import data/metadata.{type Metadata}
import data/signature.{type Signature}
import gleam/decoder_extra as decode_
import gleam/dynamic
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

pub fn decode(dyn) {
  dynamic.decode8(
    TypeSearch,
    dynamic.field("type_name", dynamic.string),
    dynamic.field("documentation", dynamic.string),
    dynamic.field("signature_kind", kind.decode),
    dynamic.field("metadata", decode_.json(metadata.decode)),
    dynamic.field("json_signature", decode_.json(signature.decode)),
    dynamic.field("module_name", dynamic.string),
    dynamic.field("package_name", dynamic.string),
    dynamic.field("version", dynamic.string),
  )(dyn)
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
