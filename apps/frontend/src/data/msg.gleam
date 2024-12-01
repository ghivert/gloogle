import birl
import data/package
import data/search_result.{type SearchResults}
import frontend/router
import gleam/dynamic.{type Dynamic}
import gleam/option
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
  ApiReturnedAnalytics(Result(Analytics, http.HttpError))
  ApiReturnedPackages(packages: Result(List(package.Package), http.HttpError))
  ApiReturnedSearchResults(
    input: String,
    result: Result(SearchResults, http.HttpError),
  )
  ApiReturnedTrendings(result: Result(List(package.Package), http.HttpError))
  BrowserChangedRoute(router.Route)
  BrowserResizedViewport(is_mobile: Bool)
  UserClickedSidebarName(String)
  UserFocusedSearch(event: Dynamic)
  UserInputtedSearch(String)
  UserPressedEscape
  UserSubmittedSearch
  UserToggledFilter(Filter, Bool)
}
