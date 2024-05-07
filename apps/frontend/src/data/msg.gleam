import data/decoders/search_result.{type SearchResult}
import lustre_http as http

pub type Msg {
  None
  SubmitSearch
  SearchResults(Result(List(SearchResult), http.HttpError))
  UpdateInput(String)
  Reset
}
