import data/analytics
import data/package
import data/search_result.{type SearchResults}
import frontend/discuss
import frontend/router
import gleam/dynamic.{type Dynamic}

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
  ApiReturnedAnalytics(analytics: analytics.Analytics)
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
