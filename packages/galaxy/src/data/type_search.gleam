import data/ir
import data/kind.{type Kind}
import data/metadata.{type Metadata}
import decrypt
import gleam/dynamic/decode
import gleam/json

/// One search result, as every tier of the cascade returns it and as venus
/// renders it. The structural part is the stored IR (`signature_ir`), decoded
/// once here: jupiter unifies it (phase 2 of the type tier) and venus renders
/// it. The display widths the legacy `signature_json` carried are recomputed at
/// render time.
pub type TypeSearch {
  TypeSearch(
    type_name: String,
    documentation: String,
    signature_kind: Kind,
    metadata: Metadata,
    signature_ir: ir.Declaration,
    module_name: String,
    package_name: String,
    version: String,
  )
}

/// Decodes a row from pog (jsonb columns arrive as strings) as well as the
/// JSON jupiter serves (`decrypt.json` accepts both).
pub fn decoder() {
  use type_name <- decode.field("type_name", decode.string)
  use documentation <- decode.field("documentation", decode.string)
  use signature_kind <- decode.field("signature_kind", kind.decoder())
  use metadata <- decode.field("metadata", decrypt.json(metadata.decoder()))
  use signature_ir <- decode.field("signature_ir", decrypt.json(ir.decoder()))
  use module_name <- decode.field("module_name", decode.string)
  use package_name <- decode.field("package_name", decode.string)
  use version <- decode.field("version", decode.string)
  decode.success({
    TypeSearch(
      type_name:,
      documentation:,
      signature_kind:,
      metadata:,
      signature_ir:,
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
    #("signature_ir", ir.encode(item.signature_ir)),
    #("module_name", json.string(item.module_name)),
    #("package_name", json.string(item.package_name)),
    #("version", json.string(item.version)),
  ])
}
