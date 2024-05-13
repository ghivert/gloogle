import data/kind.{type Kind}
import data/metadata.{type Metadata}
import data/signature.{type Signature}
import gleam/dynamic
import gleam/function
import gleam/string

pub type SearchResult {
  SearchResult(
    documentation: String,
    module_name: String,
    name: String,
    kind: Kind,
    package_name: String,
    json_signature: Signature,
    metadata: Metadata,
    version: String,
  )
}

pub type SearchResults {
  Start
  SearchResults(exact_matches: List(SearchResult), matches: List(SearchResult))
  NoSearchResults
}

pub fn decode_search_result(dyn) {
  dynamic.decode8(
    SearchResult,
    dynamic.field("documentation", dynamic.string),
    dynamic.field("module_name", dynamic.string),
    dynamic.field("name", dynamic.string),
    dynamic.field("kind", kind.decode_kind),
    dynamic.field("package_name", dynamic.string),
    dynamic.field("json_signature", signature.decode_signature),
    dynamic.field("metadata", metadata.decode_metadata),
    dynamic.field("version", dynamic.string),
  )(dyn)
}

pub fn decode_search_results(dyn) {
  dynamic.any([
    dynamic.decode1(fn(_) { NoSearchResults }, {
      dynamic.field("error", dynamic.string)
    }),
    dynamic.decode2(
      SearchResults,
      dynamic.field("exact-matches", dynamic.list(decode_search_result)),
      dynamic.field("matches", dynamic.list(decode_search_result)),
    ),
  ])(dyn)
}

pub fn hexdocs_link(search_result: SearchResult) {
  let join = function.flip(string.join)
  let base =
    join("/", [
      "https://hexdocs.pm",
      search_result.package_name,
      search_result.version,
      search_result.module_name,
    ])
  base <> ".html#" <> search_result.name
}
