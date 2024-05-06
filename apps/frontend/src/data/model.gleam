import data/decoders/search_result.{type SearchResult}

pub type Model {
  Model(input: String, search_results: List(SearchResult))
}

pub fn init() {
  Model(input: "", search_results: [])
}

pub fn update_input(model: Model, content: String) {
  Model(..model, input: content)
}

pub fn update_search_results(model: Model, search_results: List(SearchResult)) {
  Model(..model, search_results: search_results)
}
