import data/type_search.{type TypeSearch}
import frontend/view/helpers
import gleam/dynamic

pub type SearchResults {
  Start
  InternalServerError
  SearchResults(
    exact_type_matches: List(TypeSearch),
    exact_name_matches: List(TypeSearch),
    name_signature_matches: List(TypeSearch),
    vector_signature_searches: List(TypeSearch),
    docs_searches: List(TypeSearch),
    module_searches: List(TypeSearch),
  )
}

pub fn decode_search_results_list(dyn) {
  dynamic.list(type_search.decode)(dyn)
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

pub fn hexdocs_link(search_result: TypeSearch) {
  helpers.hexdocs_link(
    package: search_result.package_name,
    version: search_result.version,
    module: search_result.module_name,
    name: search_result.type_name,
  )
}
