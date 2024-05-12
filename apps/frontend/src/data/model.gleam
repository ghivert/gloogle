import data/decoders/search_result.{type SearchResults}

pub type Model {
  Model(input: String, search_results: SearchResults)
}

pub fn init() {
  Model(input: "", search_results: search_result.Start)
}

pub fn update_input(model: Model, content: String) {
  Model(..model, input: content)
}

pub fn update_search_results(model: Model, search_results: SearchResults) {
  Model(..model, search_results: search_results)
}

pub fn reset(_model: Model) {
  Model(search_results: search_result.Start, input: "")
}
