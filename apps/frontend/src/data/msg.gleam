import data/package
import data/search_result.{type SearchResults}
import frontend/router
import lustre_http as http

pub type Msg {
  None
  SubmitSearch
  SearchResults(input: String, result: Result(SearchResults, http.HttpError))
  Trendings(result: Result(List(package.Package), http.HttpError))
  UpdateInput(String)
  UpdateFilters(String)
  Reset
  ScrollTo(String)
  OnRouteChange(router.Route)
}
