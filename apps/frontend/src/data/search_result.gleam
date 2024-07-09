import data/kind.{type Kind}
import data/metadata.{type Metadata}
import data/signature.{type Signature}
import frontend/view/helpers
import gleam/dynamic
import gleam/list
import gleam/option.{None, Some}
import gleam/result

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
  InternalServerError
  SearchResults(
    exact_type_matches: List(SearchResult),
    exact_matches: List(SearchResult),
    matches: List(SearchResult),
    signature_searches: List(SearchResult),
    docs_searches: List(SearchResult),
    module_searches: List(SearchResult),
  )
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
  |> result.map(Some)
  |> result.try_recover(fn(_) { Ok(None) })
}

pub fn decode_search_results_list(dyn) {
  use data <- result.map(dynamic.list(decode_search_result)(dyn))
  use item <- list.filter_map(data)
  option.to_result(item, "")
}

pub fn decode_search_results(dyn) {
  dynamic.any([
    dynamic.decode1(fn(_) { InternalServerError }, {
      dynamic.field("error", dynamic.string)
    }),
    dynamic.decode6(
      SearchResults,
      dynamic.field("exact-type-matches", decode_search_results_list),
      dynamic.field("exact-matches", decode_search_results_list),
      dynamic.field("matches", decode_search_results_list),
      dynamic.field("searches", decode_search_results_list),
      dynamic.field("docs-searches", decode_search_results_list),
      dynamic.field("module-searches", decode_search_results_list),
    ),
  ])(dyn)
}

pub fn hexdocs_link(search_result: SearchResult) {
  helpers.hexdocs_link(
    package: search_result.package_name,
    version: search_result.version,
    module: search_result.module_name,
    name: search_result.name,
  )
}
