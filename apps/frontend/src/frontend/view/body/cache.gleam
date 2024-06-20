import data/implementations
import data/kind
import data/msg
import data/search_result
import frontend/colors/palette
import frontend/strings as frontend_strings
import frontend/view/body/signature
import frontend/view/body/styles as s
import frontend/view/documentation
import frontend/view/types as t
import gleam/bool
import gleam/dict
import gleam/dynamic
import gleam/io
import gleam/list
import gleam/option
import lustre
import lustre/attribute as a
import lustre/effect as eff
import lustre/element as el
import lustre/element/html as h
import lustre/event as e

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
        let #(content, _, background, color) = item
        s.implementations_pill(background, color, [], [h.text(content)])
      })
      |> s.implementations_pill_wrapper([], _)
  }
}

fn view_search_results(search_results: List(search_result.SearchResult)) {
  el.fragment({
    use item <- list.map(search_results)
    let package_id = item.package_name <> "@" <> item.version
    let id = package_id <> "-" <> item.module_name <> "-" <> item.name
    s.search_result([a.id(id)], [
      s.search_details([], [
        s.search_details_title([], [
          h.text(kind.display_kind(item.kind)),
          item.metadata.implementations
            |> option.map(implementations_pill)
            |> option.unwrap(el.none()),
        ]),
        s.qualified_name(
          [
            a.target("_blank"),
            a.rel("noreferrer"),
            a.href(search_result.hexdocs_link(item)),
          ],
          [
            t.dark_white(package_id),
            t.dark_white("."),
            t.keyword(item.module_name),
            t.dark_white("."),
            t.fun(item.name),
          ],
        ),
      ]),
      s.search_body([], [
        s.signature([], signature.view_signature(item)),
        case item.documentation {
          "" -> el.none()
          _ ->
            s.documentation([], [
              s.documentation_title([], [h.text("Documentation")]),
              documentation.view(item.documentation),
            ])
        },
      ]),
    ])
  })
}

fn match_title(results: List(a), title: String, content: String) {
  use <- bool.guard(when: list.is_empty(results), return: el.none())
  s.matches_titles([], [
    s.matches_title([], [h.text(title)]),
    h.div([], [h.text(content)]),
  ])
}

fn sidebar(index: List(#(#(String, String), List(#(String, String))))) {
  s.sidebar_wrapper([], {
    use #(package, modules) <- list.map(index)
    s.sidebar_package_wrapper([], [
      s.sidebar_package_name([], [
        h.text(package.0),
        t.dark_white("@" <> package.1),
      ]),
      ..list.map(modules, fn(module) {
        let #(module, name) = module
        let id = package.0 <> "@" <> package.1 <> "-" <> module <> "-" <> name
        s.sidebar_module_name([e.on_click(msg.ScrollTo(id))], [
          t.keyword(module),
          h.text("."),
          t.fun(name),
        ])
      })
    ])
  })
}

pub fn cache_search_results(
  index: List(#(#(String, String), List(#(String, String)))),
  exact: List(search_result.SearchResult),
  others: List(search_result.SearchResult),
  searches: List(search_result.SearchResult),
  docs_searches: List(search_result.SearchResult),
  modules_searches: List(search_result.SearchResult),
) {
  s.search_results_wrapper([], [
    sidebar(index),
    s.items_wrapper([], [
      match_title(exact, "Exact matches", frontend_strings.exact_match),
      view_search_results(exact),
      match_title(others, "Signature matches", frontend_strings.partial_match),
      view_search_results(others),
      match_title(searches, "Searches matches", frontend_strings.searches_match),
      view_search_results(searches),
      match_title(
        docs_searches,
        "Documentation matches",
        frontend_strings.docs_match,
      ),
      view_search_results(docs_searches),
      match_title(
        modules_searches,
        "Module matches",
        frontend_strings.modules_match,
      ),
      view_search_results(modules_searches),
    ]),
  ])
}

pub type MsgComponent {
  UpdateContent(el.Element(MsgComponent))
}

pub fn component() {
  lustre.component(
    fn(_flags) { #(el.none(), eff.none()) },
    fn(_model, msg) {
      case msg {
        UpdateContent(c) -> #(c, eff.none())
      }
    },
    fn(model) { model },
    dict.from_list([
      #("content", fn(dyn) { Ok(UpdateContent(dynamic.unsafe_coerce(dyn))) }),
    ]),
  )
}
