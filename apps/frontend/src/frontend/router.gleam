import frontend/ffi
import gleam/list
import gleam/option
import gleam/result
import gleam/uri.{type Uri}
import lustre/effect

pub type Route {
  Home
  Search(query: String)
  Packages
  Trending
  Analytics
}

pub fn parse_uri(uri: Uri) -> Route {
  case uri.path_segments(uri.path) {
    ["search"] -> handle_search_path(uri)
    ["packages"] -> Packages
    ["trending"] -> Trending
    ["analytics"] -> Analytics
    _ -> Home
  }
}

fn handle_search_path(uri: Uri) {
  uri.query
  |> option.map(uri.parse_query)
  |> option.then(fn(query_params) {
    query_params
    |> result.unwrap([])
    |> list.key_find("q")
    |> option.from_result()
  })
  |> option.map(Search)
  |> option.unwrap(Home)
}

pub fn update_page_title(route: Route) {
  use _ <- effect.from()
  case route {
    Home -> ffi.update_title("Gloogle")
    Packages -> ffi.update_title("Gloogle — Packages")
    Search(q) -> ffi.update_title("Gloogle — Search " <> q)
    Trending -> ffi.update_title("Gloogle — Trending")
    Analytics -> ffi.update_title("Gloogle — Analytics")
  }
}
