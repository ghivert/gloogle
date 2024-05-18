import frontend/colors/palette
import gleam/bool
import sketch as s
import sketch/lustre/extra as l
import sketch/size.{px}

pub fn search_input_wrapper(loading: Bool, children) {
  let id = "search-input-wrapper-" <> bool.to_string(loading)
  l.dynamic("div", [], children, id, [
    s.border_radius(px(18)),
    s.overflow("hidden"),
    s.grid_area("input"),
    s.padding(
      px(case loading {
        True -> 4
        False -> 0
      }),
    ),
    s.background("linear-gradient(-45deg, #4ce7ff, #c651e5, #e3d8be, #4ce7ff)"),
    s.property("background-size", "400% 400%"),
    s.transition("padding .3s"),
    s.animation("bg-spin 3s linear infinite"),
    s.animation_play_state(case loading {
      True -> "running"
      False -> "paused"
    }),
  ])
}

pub fn search_input(loading, children) {
  let id_ = "search-input-" <> bool.to_string(loading)
  l.dynamic("div", [], children, id_, [
    s.display("flex"),
    s.gap(px(6)),
    s.border_radius(px(14)),
    s.color(palette.dark.charcoal),
    s.background(palette.dark.white),
    s.transition("padding .3s"),
    s.align_items("baseline"),
    s.padding(
      px(case loading {
        True -> 16
        False -> 20
      }),
    ),
  ])
}

pub fn search_input_content(attributes) {
  l.element("input", attributes, [], [
    s.appearance("none"),
    s.border("none"),
    s.outline("none"),
    s.width(size.percent(100)),
    s.line_height("1.5"),
  ])
}
