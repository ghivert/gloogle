import gleam/list
import gleam/option
import gleam/uri
import venus/router

/// The URL pushed for a search must read back as the same search, whatever
/// the query contains — `#` starts a fragment, `&` separates parameters and
/// `+` decodes to a space when left unencoded (ghivert/gloogle#21).
pub fn search_round_trips_through_its_url_test() {
  [
    "fn(#(a, b)) -> a",
    "a & b + c",
    "100% ?x=y",
    "List(a) -> Int",
    "",
  ]
  |> list.each(fn(query) {
    let route = router.Search(query)
    let assert Ok(parsed) =
      route
      |> router.to_uri
      |> uri.to_string
      |> uri.parse
    assert router.parse_uri(parsed) == route
  })
}

pub fn search_uri_has_no_fragment_test() {
  let uri = router.to_uri(router.Search("fn(#(a, b)) -> a"))
  assert uri.path == "/search"
  assert uri.fragment == option.None
}
