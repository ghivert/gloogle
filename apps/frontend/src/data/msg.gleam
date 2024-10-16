import birl
import data/package
import data/search_result.{type SearchResults}
import frontend/router
import gleam/option
import lustre_http as http
import plinth/browser/event.{type Event}

pub type Filter {
  Functions
  Types
  Aliases
  Documented
  ShowOldPackages
  DocumentationSearch
  VectorSearch
}

pub type Package {
  Package(
    name: String,
    repository: String,
    rank: Int,
    popularity: option.Option(Int),
  )
}

pub type Analytics {
  Analytics(
    total_searches: Int,
    total_signatures: Int,
    total_indexed: Int,
    timeseries: List(#(Int, birl.Time)),
    ranked: List(Package),
    popular: List(Package),
  )
}

pub type Msg {
  None
  Packages(packages: Result(List(package.Package), http.HttpError))
  OnSearchFocus(event: Event)
  SubmitSearch
  UpdateIsMobile(is_mobile: Bool)
  SearchResults(input: String, result: Result(SearchResults, http.HttpError))
  Trendings(result: Result(List(package.Package), http.HttpError))
  UpdateInput(String)
  Reset
  ScrollTo(String)
  OnEscape
  OnAnalytics(Result(Analytics, http.HttpError))
  OnRouteChange(router.Route)
  OnCheckFilter(Filter, Bool)
}
