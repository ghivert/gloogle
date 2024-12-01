import birl
import data/package
import data/search_result.{type SearchResults}
import frontend/discuss
import frontend/router
import gleam/dynamic.{type Dynamic}
import gleam/option

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
  ApiReturnedAnalytics(analytics: Analytics)
  ApiReturnedPackages(packages: List(package.Package))
  ApiReturnedSearchResults(input: String, search_results: SearchResults)
  ApiReturnedTrendings(trendings: List(package.Package))
  AppRequiredDiscussToast(message: discuss.DiscussError)
  BrowserChangedRoute(route: router.Route)
  BrowserResizedViewport(is_mobile: Bool)
  UserClickedSidebarName(id: String)
  UserFocusedSearch(event: Dynamic)
  UserInputtedSearch(query: String)
  UserPressedEscape
  UserSubmittedSearch
  UserToggledFilter(filter: Filter, value: Bool)
}
