import data/msg
import frontend/view/search_input/styles as s
import gleam/list
import gleam/set
import gleam/string
import lustre/attribute as a
import lustre/element as el
import lustre/element/html as h
import lustre/event as e

pub const valid_filters = [
  "in:name", "in:signature", "in:documentation", "in:module",
]

fn active_filters(input: String) {
  input
  |> string.split(" ")
  |> list.filter(fn(word) { list.contains(valid_filters, word) })
  |> fn(l) {
    case l {
      [] -> valid_filters
      _ -> l
    }
  }
  |> set.from_list()
}

pub fn view(
  loading loading: Bool,
  input input: String,
  show_filters filters: Bool,
  small small: Bool,
) {
  let user_filters = active_filters(input)
  s.search_with_filters([], [
    s.search_input_wrapper(loading, [
      s.search_input(loading, small, [
        s.search_input_content([
          a.placeholder("Search for a function, or a type"),
          e.on_input(msg.UpdateInput),
          a.value(input),
          a.attribute("autocorrect", "off"),
          a.attribute("autocapitalize", "none"),
        ]),
      ]),
    ]),
    case filters {
      False -> el.none()
      True ->
        s.filter_pills([], {
          use filter <- list.map(valid_filters)
          let active = set.contains(user_filters, filter)
          let on_click = {
            use event <- e.on("click")
            e.prevent_default(event)
            Ok(msg.UpdateFilters(filter))
          }
          s.filter_pill(active, [on_click, a.type_("button")], [h.text(filter)])
        })
    },
  ])
}
