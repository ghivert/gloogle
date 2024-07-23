import data/implementations
import data/search_result.{type SearchResult}
import frontend/colors/palette
import frontend/icons
import frontend/view/body/signature
import frontend/view/documentation
import frontend/view/types as t
import gleam/bool
import gleam/coerce
import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/option
import gleam/result
import lustre
import lustre/attribute as a
import lustre/effect
import lustre/element
import lustre/element/html as h
import lustre/event as e
import lustre/update

pub type Model {
  Model(item: option.Option(SearchResult), opened: Bool)
}

pub type Msg {
  Received(option.Option(SearchResult))
  ToggleOpen
}

fn on_attribute_change() {
  let on_coerce = fn(dyn) { Ok(coerce.coerce(dyn)) } |> dynamic.optional
  dict.from_list([#("item", fn(dyn) { on_coerce(dyn) |> result.map(Received) })])
}

pub fn setup() {
  let attrs = on_attribute_change()
  let init = fn(_) { #(Model(item: option.None, opened: False), effect.none()) }
  lustre.component(init, update, internal_view, attrs)
  |> lustre.register("search-result")
}

pub fn view(item: SearchResult) {
  let attributes = [a.property("item", item)]
  element.element("search-result", attributes, [])
}

fn update(model, msg) {
  case msg {
    ToggleOpen -> Model(..model, opened: !model.opened)
    Received(search_result) -> Model(item: search_result, opened: False)
  }
  |> update.none
}

fn implementation_pill(item) {
  let #(content, _, background, _) = item
  let style = a.style([#("background", background)])
  h.div([a.class("implementations-pill-container")], [
    h.div([a.class("implementations-pill"), style], []),
    h.text(content),
  ])
}

fn implementation_pills(implementations: implementations.Implementations) {
  case implementations {
    implementations.Implementations(True, False, False) -> element.none()
    implementations.Implementations(gleam, erl, js) ->
      [
        #("Gleam", gleam, palette.dark.faff_pink, palette.dark.blacker),
        #("Erlang", erl, palette.erlang, palette.dark.white),
        #("JavaScript", js, palette.javascript, palette.dark.blacker),
      ]
      |> list.filter(fn(item) { item.1 })
      |> list.map(implementation_pill)
      |> h.div([a.class("implementations-pill-wrapper")], _)
  }
}

fn internal_view(model: Model) -> element.Element(Msg) {
  use <- bool.guard(when: option.is_none(model.item), return: element.none())
  let assert option.Some(item) = model.item
  let package_id = item.package_name <> "@" <> item.version
  let id = package_id <> "-" <> item.module_name <> "-" <> item.name
  h.div([a.class("search-result"), a.id(id)], [
    h.div([a.class("search-details")], [
      h.div([a.class("search-details-name")], [
        view_name(item),
        h.div([a.class("external-icon-wrapper")], [icons.external_link()]),
      ]),
      view_documentation_arrow(model, item),
    ]),
    h.div([a.class("search-body")], [
      h.code([a.class("signature")], signature.view_signature(item)),
    ]),
    view_implementation_pills(model, item),
    view_documentation(model, item),
  ])
}

fn view_name(item: SearchResult) {
  let class = a.class("qualified-name")
  let href = search_result.hexdocs_link(item)
  h.a([class, a.target("_blank"), a.rel("noreferrer"), a.href(href)], [
    t.white(item.package_name),
    t.dark_white("@" <> item.version),
    t.dark_white("."),
    t.keyword(item.module_name),
    t.dark_white("."),
    t.fun(item.name),
  ])
}

fn view_documentation_arrow(model: Model, item: SearchResult) {
  use <- bool.guard(when: item.documentation == "", return: element.none())
  let no_implementation = option.is_none(item.metadata.implementations)
  use <- bool.guard(when: no_implementation, return: element.none())
  let class = a.class("search-details-arrow-expand")
  let data_opened = a.attribute("data-opened", bool.to_string(model.opened))
  h.button([class, data_opened, e.on_click(ToggleOpen)], [
    h.span([], [
      element.text(
        case model.opened {
          True -> "Hide"
          False -> "Show"
        }
        <> " documentation",
      ),
    ]),
    icons.arrow(),
  ])
}

fn view_implementation_pills(model: Model, item: SearchResult) {
  use <- bool.guard(when: !model.opened, return: element.none())
  item.metadata.implementations
  |> option.map(implementation_pills)
  |> option.unwrap(element.none())
}

fn view_documentation(model: Model, item: SearchResult) {
  use <- bool.guard(when: item.documentation == "", return: element.none())
  use <- bool.guard(when: !model.opened, return: element.none())
  h.div([a.class("documentation")], [documentation.view(item.documentation)])
}
