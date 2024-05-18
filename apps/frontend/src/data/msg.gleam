import data/search_result.{type SearchResults}
import frontend/router
import lustre_http as http

pub type Msg {
  None
  SubmitSearch
  SearchResults(input: String, result: Result(SearchResults, http.HttpError))
  UpdateInput(String)
  Reset
  ScrollTo(String)
  OnRouteChange(router.Route)
}
