import data/implementations
import data/search_result
import frontend/colors/palette
import frontend/icons
import frontend/view/body/signature
import frontend/view/documentation
import frontend/view/types as t
import gleam/bool
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

pub fn setup() {
  let attrs = on_attribute_change()
  lustre.component(init, update, view, attrs)
  |> lustre.register("search-result")
}

fn implementations_pill(implementations: implementations.Implementations) {
  case implementations {
    implementations.Implementations(True, False, False) -> element.none()
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

pub type Model {
  Model(item: option.Option(search_result.SearchResult), opened: Bool)
}

pub type Msg {
  Received(option.Option(search_result.SearchResult))
  ToggleOpen
}

fn init(_) {
  #(Model(item: option.None, opened: False), effect.none())
}

fn update(model, msg) {
  case msg {
    ToggleOpen -> #(Model(..model, opened: !model.opened), effect.none())
    Received(search_result) -> #(
      Model(..model, item: search_result, opened: False),
      effect.none(),
    )
  }
}

fn view(model: Model) -> element.Element(Msg) {
  case model.item {
    option.None -> element.text("bloup")
    option.Some(item) -> {
      let package_id = item.package_name <> "@" <> item.version
      let id = package_id <> "-" <> item.module_name <> "-" <> item.name
      h.div([a.class("search-result"), a.id(id)], [
        h.div([a.class("search-details")], [
          h.div([a.class("search-details-name")], [
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
          case item.documentation, item.metadata.implementations {
            "", option.None -> element.none()
            _, _ ->
              h.button(
                [
                  a.class("search-details-arrow-expand"),
                  a.attribute("data-opened", bool.to_string(model.opened)),
                  e.on_click(ToggleOpen),
                ],
                [
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
                ],
              )
          },
        ]),
        h.div([a.class("search-body")], [
          h.code([a.class("signature")], signature.view_signature(item)),
        ]),
        case model.opened {
          False -> element.none()
          True ->
            item.metadata.implementations
            |> option.map(implementations_pill)
            |> option.unwrap(element.none())
        },
        case model.opened, item.documentation {
          False, _ | True, "" -> element.none()
          True, _ ->
            h.div([a.class("documentation")], [
              documentation.view(item.documentation),
            ])
        },
      ])
    }
  }
}

pub fn on_attribute_change() {
  dict.from_list([
    #("item", {
      fn(dyn) {
        {
          fn(dyn) { Ok(dynamic.unsafe_coerce(dyn)) }
          |> dynamic.optional()
        }(dyn)
        |> result.map(Received)
      }
    }),
  ])
}
