import data/decoders/implementations.{type Implementations, Implementations}
import data/decoders/nature.{type Nature}
import data/decoders/signature.{type Signature}
import gleam/dynamic
import gleam/option.{type Option}
import gleam/result

pub type Metadata {
  Metadata(
    deprecation: Option(String),
    implementations: Option(Implementations),
  )
}

fn completely_option(dyn) {
  dynamic.optional_field("deprecation", dynamic.optional(dynamic.string))(dyn)
  |> result.map(fn(res) { option.flatten(res) })
}

fn decode_metadata(dyn) {
  dynamic.decode2(
    Metadata,
    completely_option,
    dynamic.optional_field(
      "implementations",
      dynamic.decode3(
        Implementations,
        dynamic.field("gleam", dynamic.bool),
        dynamic.field("uses_erlang_externals", dynamic.bool),
        dynamic.field("uses_javascript_externals", dynamic.bool),
      ),
    ),
  )(dyn)
}

pub type SearchResult {
  SearchResult(
    documentation: String,
    module_name: String,
    name: String,
    nature: Nature,
    package_name: String,
    json_signature: Signature,
    metadata: Metadata,
    version: String,
  )
}

pub fn decode_search_result(dyn) {
  dynamic.decode8(
    SearchResult,
    dynamic.field("documentation", dynamic.string),
    dynamic.field("module_name", dynamic.string),
    dynamic.field("name", dynamic.string),
    dynamic.field("nature", nature.decode_nature),
    dynamic.field("package_name", dynamic.string),
    dynamic.field("json_signature", signature.decode_signature),
    dynamic.field("metadata", decode_metadata),
    dynamic.field("version", dynamic.string),
  )(dyn)
}

pub fn decode_search_results(dyn) {
  dynamic.list(decode_search_result)(dyn)
}
