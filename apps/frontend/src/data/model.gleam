import data/search_result.{type SearchResult, type SearchResults}
import gleam/list
import gleam/pair
import gleam/result

pub type Index =
  List(#(#(String, String), List(#(String, String))))

pub type Model {
  Model(input: String, search_results: SearchResults, index: Index)
}

pub fn init() {
  let search_results = search_result.Start
  let index = compute_index(search_results)
  Model(input: "", search_results: search_results, index: index)
}

pub fn update_input(model: Model, content: String) {
  Model(..model, input: content)
}

pub fn update_search_results(model: Model, search_results: SearchResults) {
  let index = compute_index(search_results)
  Model(..model, search_results: search_results, index: index)
}

pub fn reset(_model: Model) {
  Model(search_results: search_result.Start, input: "", index: [])
}

fn compute_index(search_results: SearchResults) -> Index {
  case search_results {
    search_result.Start -> []
    search_result.NoSearchResults -> []
    search_result.SearchResults(exact, others) -> {
      []
      |> insert_module_names(exact)
      |> insert_module_names(others)
      |> list.map(fn(i) { pair.map_second(i, list.reverse) })
    }
  }
}

fn insert_module_names(index: Index, search_results: List(SearchResult)) {
  use acc, val <- list.fold(search_results, index)
  let key = #(val.package_name, val.version)
  list.key_find(acc, key)
  |> result.unwrap([])
  |> fn(i) { list.prepend(i, #(val.module_name, val.name)) }
  |> fn(i) { list.key_set(acc, key, i) }
}
