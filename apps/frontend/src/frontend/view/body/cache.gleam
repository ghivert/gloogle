import data/implementations
import data/kind
import data/msg
import data/search_result
import frontend/colors/palette
import frontend/icons
import frontend/strings as frontend_strings
import frontend/view/body/signature
import frontend/view/documentation
import frontend/view/types as t
import gleam/bool
import gleam/coerce.{coerce}
import gleam/dict
import gleam/io
import gleam/list
import gleam/option
import lustre
import lustre/attribute as a
import lustre/effect as eff
import lustre/element as el
import lustre/element/html as h
import lustre/event as e
import sketch/lustre as sketch_lustre
import sketch/options as sketch_options

fn view_search_results(search_results: List(search_result.SearchResult)) {
  el.fragment({
    list.map(search_results, fn(item) {
      el.element("search-result", [a.property("item", item)], [])
    })
    |> list.intersperse(h.div([a.class("search-result-separator")], []))
  })
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
          h.div([a.class("sidebar-module-name"), e.on_click(msg.ScrollTo(id))], [
            t.keyword(module),
            h.text("."),
            t.fun(name),
          ])
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
      view_search_results(types),
      maybe_separator(types),
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
