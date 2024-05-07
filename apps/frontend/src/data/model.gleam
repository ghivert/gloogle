import data/decoders/search_result.{type SearchResult}
import data/model/mock
import gleam/result

pub type Model {
  Model(input: String, search_results: List(SearchResult))
}

pub fn init() {
  let search_results = result.unwrap(mock.mock(), [])
  Model(input: "", search_results: search_results)
}

pub fn update_input(model: Model, content: String) {
  Model(..model, input: content)
}

pub fn update_search_results(model: Model, search_results: List(SearchResult)) {
  Model(..model, search_results: search_results)
}
