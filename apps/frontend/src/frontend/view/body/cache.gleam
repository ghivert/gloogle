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

fn implementations_pill(implementations: implementations.Implementations) {
  case implementations {
    implementations.Implementations(True, False, False) -> el.none()
    implementations.Implementations(gleam, erl, js) ->
      [
        #("Gleam", gleam, palette.dark.faff_pink, palette.dark.blacker),
        #("Erlang", erl, palette.erlang, palette.dark.white),
        #("JavaScript", js, palette.javascript, palette.dark.blacker),
      ]
      |> list.filter(fn(item) { item.1 })
      |> list.map(fn(item) {
        let #(content, _, background, _) = item
        h.div([a.class("implementations-pill-container")], [
          h.div(
            [
              a.class("implementations-pill"),
              a.style([#("background", background)]),
            ],
            [],
          ),
          h.text(content),
        ])
      })
      |> h.div([a.class("implementations-pill-wrapper")], _)
  }
}

fn view_search_results(search_results: List(search_result.SearchResult)) {
  el.fragment({
    list.map(search_results, fn(item) {
      let package_id = item.package_name <> "@" <> item.version
      let id = package_id <> "-" <> item.module_name <> "-" <> item.name
      h.div([a.class("search-result"), a.id(id)], [
        h.div([a.class("search-details")], [
          h.a(
            [
              a.class("qualified-name"),
              a.target("_blank"),
              a.rel("noreferrer"),
              a.href(search_result.hexdocs_link(item)),
            ],
            [
              t.white(item.package_name),
              t.dark_white("@" <> item.version),
              t.dark_white("."),
              t.keyword(item.module_name),
              t.dark_white("."),
              t.fun(item.name),
            ],
          ),
          h.div([a.class("external-icon-wrapper")], [icons.external_link()]),
        ]),
        h.div([a.class("search-body")], [
          h.code([a.class("signature")], signature.view_signature(item)),
        ]),
        item.metadata.implementations
          |> option.map(implementations_pill)
          |> option.unwrap(el.none()),
        case item.documentation {
          "" -> el.none()
          _ ->
            h.div([a.class("documentation")], [
              documentation.view(item.documentation),
            ])
        },
      ])
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
        h.div([a.class("matches-title")], [h.text("Search results")]),
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

pub type MsgComponent(msg) {
  UpdateContent(el.Element(msg))
  Received(msg)
}

@external(javascript, "../../../config.ffi.mjs", "coerce")
fn coerce(value: a) -> b

pub fn component() {
  lustre.component(
    fn(_flags) { #(el.none(), eff.none()) },
    fn(model: el.Element(msg), msg: MsgComponent(msg)) {
      case msg {
        UpdateContent(c) -> #(c, eff.none())
        Received(msg) -> #(model, e.emit("child", coerce(msg)))
      }
    },
    fn(model) { el.map(model, Received) },
    dict.from_list([#("content", fn(dyn) { Ok(UpdateContent(coerce(dyn))) })]),
  )
}
