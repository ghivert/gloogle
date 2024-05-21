import gleam/list
import gleam/option
import gleam/result
import gleam/uri.{type Uri}

pub type Route {
  Home
  Search(query: String)
  Trending
}

pub fn parse_uri(uri: Uri) -> Route {
  case uri.path_segments(uri.path) {
    ["search"] -> handle_search_path(uri)
    ["trending"] -> Trending
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
