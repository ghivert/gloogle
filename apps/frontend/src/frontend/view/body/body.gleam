import data/implementations
import data/kind
import data/model.{type Index, type Model}
import data/msg
import data/search_result
import frontend/colors/palette
import frontend/documentation
import frontend/images
import frontend/strings as frontend_strings
import frontend/view/body/signature
import frontend/view/body/styles as s
import frontend/view/types as t
import gleam/bool
import gleam/list
import gleam/option
import lustre/attribute as a
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
        s.qualified_name([], [
          t.dark_white(package_id),
          t.dark_white("."),
          t.keyword(item.module_name),
          t.dark_white("#"),
          t.fun(item.name),
        ]),
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

fn view_search_input(model: Model) {
  s.search_wrapper([e.on_submit(msg.SubmitSearch)], [
    s.search_title_wrapper([], [
      s.search_title([], [
        s.search_lucy([a.src("/images/lucy.svg")]),
        h.text("Gloogle"),
      ]),
      h.text(frontend_strings.gloogle_description),
    ]),
    s.search_input([
      a.placeholder("Search for a function, or a type"),
      e.on_input(msg.UpdateInput),
      a.value(model.input),
    ]),
    s.search_submit([a.type_("submit"), a.value("Submit")]),
  ])
}

fn match_title(results: List(a), title: String, content: String) {
  use <- bool.guard(when: list.is_empty(results), return: el.none())
  s.matches_titles([], [
    s.matches_title([], [h.text(title)]),
    h.div([], [h.text(content)]),
  ])
}

fn empty_state(
  image image: String,
  title title: String,
  content content: String,
) {
  s.empty_state([], [
    s.empty_state_lucy([a.src(image)]),
    s.empty_state_titles([], [
      h.div([], [h.text(title)]),
      s.empty_state_subtitle([], [h.text(content)]),
    ]),
  ])
}

fn sidebar(index: Index) {
  s.sidebar_wrapper([], {
    use #(package, modules) <- list.map(index)
    s.sidebar_package_wrapper([], [
      s.sidebar_package_name([], [
        h.text(package.0),
        t.dark_white("#" <> package.1),
      ]),
      ..list.map(modules, fn(module) {
        let #(module, name) = module
        let id = package.0 <> "@" <> package.1 <> "-" <> module <> "-" <> name
        s.sidebar_module_name([e.on_click(msg.ScrollTo(id))], [
          t.keyword(module),
          h.text("#"),
          t.fun(name),
        ])
      })
    ])
  })
}

pub fn body(model: Model) {
  s.main([], [
    case model.search_results {
      search_result.Start -> view_search_input(model)
      search_result.NoSearchResults ->
        empty_state(
          image: images.internal_error,
          title: "Internal server error",
          content: frontend_strings.internal_server_error,
        )
      search_result.SearchResults([], []) ->
        empty_state(
          image: images.shadow_lucy,
          title: "No match found!",
          content: frontend_strings.retry_query,
        )
      search_result.SearchResults(exact, others) ->
        s.search_results_wrapper([], [
          sidebar(model.index),
          s.items_wrapper([], [
            match_title(exact, "Exact matches", frontend_strings.exact_match),
            view_search_results(exact),
            match_title(others, "Other matches", frontend_strings.partial_match),
            view_search_results(others),
          ]),
        ])
    },
  ])
}
