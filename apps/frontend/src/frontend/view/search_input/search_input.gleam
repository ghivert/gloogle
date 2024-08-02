import data/msg
import frontend/view/search_input/styles as s
import lustre/attribute as a
import lustre/element as el
import lustre/event as e

pub fn view(loading loading: Bool, input input: String, small small: Bool) {
  s.search_with_filters([], [
    s.search_input_wrapper(loading, [
      s.search_input(loading, small, [
        s.search_input_content([
          a.id("search-input"),
          a.placeholder("Search for a function, or a type"),
          e.on_input(msg.UpdateInput),
          a.value(input),
          a.attribute("autocorrect", "off"),
          a.attribute("autocapitalize", "none"),
          a.attribute("autofocus", "autofocus"),
        ]),
        s.shortcut_hint([], [el.text("s")]),
      ]),
    ]),
  ])
}
