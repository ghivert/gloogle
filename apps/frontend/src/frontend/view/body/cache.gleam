import data/msg
import data/search_result
import frontend/view/body/search_result as sr
import frontend/view/types as t
import gleam/list
import lustre/attribute as a
import lustre/element as el
import lustre/element/html as h
import lustre/event as e

fn view_search_results(search_results: List(search_result.SearchResult)) {
  list.map(search_results, sr.view)
  |> list.intersperse(h.div([a.class("search-result-separator")], []))
  |> el.fragment
}

fn sidebar(
  search: String,
  index: List(#(#(String, String), List(#(String, String)))),
) {
  h.div([a.class("sidebar-wrapper")], [
    h.div([a.class("sidebar-wrapper-title")], [
      el.text("Packages for “" <> search <> "”"),
    ]),
    ..{
      use #(package, modules) <- list.map(index)
      h.div([a.class("sidebar-package-wrapper")], [
        h.div([a.class("sidebar-package-name")], [
          h.text(package.0),
          t.dark_white("@" <> package.1),
        ]),
        ..list.map(modules, fn(module) {
          let #(module, name) = module
          let id = package.0 <> "@" <> package.1 <> "-" <> module <> "-" <> name
          h.div(
            [
              a.class("sidebar-module-name"),
              e.on_click(msg.UserClickedSidebarName(id)),
            ],
            [t.keyword(module), h.text("."), t.fun(name)],
          )
        })
      ])
    }
  ])
}

fn maybe_separator(l) {
  case list.is_empty(l) {
    True -> el.none()
    False -> h.div([a.class("search-result-separator")], [])
  }
}

fn empty_types() {
  h.div([a.class("search-result-empty-callout")], [
    h.text("Unfortunately, your query did not find any matching function. "),
    h.text("Keep in mind you can use _ when you don't "),
    h.text("know the type you're searching for."),
    h.br([]),
    h.text("You can still take a look below, in case the vector search"),
    h.text(" returned an approximate result. Otherwise, refine your request!"),
  ])
}

fn types_separator() {
  h.div([a.class("search-result-empty-callout")], [
    h.text("Starting here, Gloogle tries to respond to your query by trying "),
    h.text("different strategies, like looking in documentation or with "),
    h.text("a vector search!"),
    h.br([]),
    h.text("Results can be approximate, but maybe you'll find what you need!"),
  ])
}

pub fn cache_search_results(
  search: String,
  index: List(#(#(String, String), List(#(String, String)))),
  types: List(search_result.SearchResult),
  exact: List(search_result.SearchResult),
  others: List(search_result.SearchResult),
  searches: List(search_result.SearchResult),
  docs_searches: List(search_result.SearchResult),
  modules_searches: List(search_result.SearchResult),
) {
  h.div([a.class("search-results-wrapper")], [
    sidebar(search, index),
    h.div([a.class("items-wrapper")], [
      h.div([a.class("matches-titles")], [
        h.div([a.class("matches-title")], [
          h.text("Search results for “" <> search <> "”"),
        ]),
      ]),
      case types {
        [] -> empty_types()
        types ->
          el.fragment([
            view_search_results(types),
            h.div([a.class("search-result-separator")], []),
            types_separator(),
          ])
      },
      h.div([a.class("search-result-separator")], []),
      view_search_results(exact),
      maybe_separator(exact),
      view_search_results(others),
      maybe_separator(others),
      view_search_results(searches),
      maybe_separator(searches),
      view_search_results(docs_searches),
      maybe_separator(docs_searches),
      view_search_results(modules_searches),
    ]),
  ])
}
