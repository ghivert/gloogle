import data/package
import data/search_result.{type SearchResults}
import frontend/router
import lustre_http as http

pub type Filter {
  Functions
  Types
  Aliases
  Documented
  ShowOldPackages
  DocumentationSearch
  VectorSearch
}

pub type Msg {
  None
  OnSearchFocus
  SubmitSearch
  UpdateIsMobile(is_mobile: Bool)
  SearchResults(input: String, result: Result(SearchResults, http.HttpError))
  Trendings(result: Result(List(package.Package), http.HttpError))
  UpdateInput(String)
  Reset
  ScrollTo(String)
  OnEscape
  OnRouteChange(router.Route)
  OnCheckFilter(Filter, Bool)
}
