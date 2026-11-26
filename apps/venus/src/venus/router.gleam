import gleam/list
import gleam/option
import gleam/result
import gleam/uri.{type Uri}
import modem
import venus/effects/document

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

/// `Search` carries the raw user query. It is percent-encoded here, never
/// concatenated: `#`, `&` or `+` in a type signature would otherwise be read
/// back as a fragment, a separator or a space.
pub fn to_uri(route: Route) -> Uri {
  let path = case route {
    Home -> "/"
    Search(..) -> "/search"
    Packages -> "/packages"
    Trending -> "/trending"
    Analytics -> "/analytics"
  }
  let query = case route {
    Search(query:) -> option.Some(uri.query_to_string([#("q", query)]))
    _ -> option.None
  }
  uri.Uri(..uri.empty, path:, query:)
}

pub fn push(route: Route) {
  let uri = to_uri(route)
  modem.push(uri.path, uri.query, uri.fragment)
}

pub fn replace(route: Route) {
  let uri = to_uri(route)
  modem.replace(uri.path, uri.query, uri.fragment)
}
