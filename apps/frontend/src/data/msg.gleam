import data/decoders/search_result.{type SearchResults}
import lustre_http as http

pub type Msg {
  None
  SubmitSearch
  SearchResults(Result(SearchResults, http.HttpError))
  UpdateInput(String)
  Reset
}
