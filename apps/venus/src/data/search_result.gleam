import data/type_search.{type TypeSearch}
import gleam/dynamic/decode
import venus/view/helpers

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

pub fn search_results_decoder() {
  decode.one_of(search_results_decoder_(), or: [
    decode.at(["error"], decode.string)
    |> decode.map(fn(_) { InternalServerError }),
  ])
}

fn search_results_decoder_() {
  let results = decode.list(type_search.decoder())
  use exact_type_matches <- decode.field("exact-type-matches", results)
  use exact_name_matches <- decode.field("exact-matches", results)
  use name_signature_matches <- decode.field("matches", results)
  use vector_signature_searches <- decode.field("searches", results)
  use docs_searches <- decode.field("docs-searches", results)
  use module_searches <- decode.field("module-searches", results)
  decode.success({
    SearchResults(
      exact_type_matches:,
      exact_name_matches:,
      name_signature_matches:,
      vector_signature_searches:,
      docs_searches:,
      module_searches:,
    )
  })
}

pub fn hexdocs_link(search_result: TypeSearch) {
  helpers.hexdocs_link(
    package: search_result.package_name,
    version: search_result.version,
    module: search_result.module_name,
    name: search_result.type_name,
  )
}
