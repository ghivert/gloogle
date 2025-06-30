import frontend/effects/document
import gleam/list
import gleam/option
import gleam/result
import gleam/uri.{type Uri}
import modem

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
  case route {
    Home -> document.update_title("Gloogle")
    Packages -> document.update_title("Gloogle — Packages")
    Search(q) -> document.update_title("Gloogle — Search " <> q)
    Trending -> document.update_title("Gloogle — Trending")
    Analytics -> document.update_title("Gloogle — Analytics")
  }
}

pub fn to_uri(route: Route) -> Uri {
  let assert Ok(uri) = case route {
    Home -> uri.parse("/")
    Search(query:) -> uri.parse("/search?" <> query)
    Packages -> uri.parse("/packages")
    Trending -> uri.parse("/trending")
    Analytics -> uri.parse("/analytics")
  }
  uri
}

pub fn push(route: Route) {
  let uri = to_uri(route)
  modem.push(uri.path, uri.query, uri.fragment)
}

pub fn replace(route: Route) {
  let uri = to_uri(route)
  modem.replace(uri.path, uri.query, uri.fragment)
}
