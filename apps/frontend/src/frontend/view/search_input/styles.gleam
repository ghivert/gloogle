import frontend/colors/palette
import gleam/bool
import sketch as s
import sketch/lustre/element as l
import sketch/media
import sketch/size.{px}

pub fn search_with_filters(attributes, children) {
  l.element("div", attributes, children, [
    s.grid_area("input"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(12)),
  ])
}

pub fn search_input_wrapper(loading: Bool, children) {
  let id = "search-input-wrapper-" <> bool.to_string(loading)
  l.dynamic("div", [], children, id, [
    s.border_radius(px(12)),
    s.overflow("hidden"),
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
    s.border("1px solid var(--border-color)"),
    s.animation_play_state(case loading {
      True -> "running"
      False -> "paused"
    }),
  ])
}

pub fn search_input(loading, small, children) {
  let id_ =
    "search-input-" <> bool.to_string(loading) <> "-" <> bool.to_string(small)
  l.dynamic("div", [], children, id_, [
    s.display("flex"),
    s.gap(px(6)),
    s.border_radius(px(8)),
    s.color("var(--input-text-color)"),
    s.background(case small {
      False -> "var(--input-background)"
      True -> "var(--dark-background)"
    }),
    s.transition("padding .3s"),
    s.align_items("baseline"),
    s.padding(
      px(case loading, small {
        True, False -> 16
        False, False -> 20
        True, True -> 6
        False, True -> 10
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
    s.background("transparent"),
    s.color("inherit"),
  ])
}

pub fn filter_pills(attributes, children) {
  l.element("div", attributes, children, [
    s.display("flex"),
    s.gap(px(12)),
    s.justify_content("end"),
    s.media(media.max_width(px(700)), [s.display("none")]),
  ])
}

pub fn filter_pill(active: Bool, attributes, children) {
  let id = "filter_pill-active-" <> bool.to_string(active)
  l.dynamic("button", attributes, children, id, [
    s.background("var(--secondary)"),
    // s.color("var(--text-color)"),
    s.padding_("3px 12px"),
    s.border_radius(px(30)),
    s.font_size(px(12)),
    s.transition("opacity .3s"),
    s.appearance("none"),
    s.border("none"),
    s.cursor("pointer"),
    s.opacity(case active {
      True -> 1.0
      False -> 0.5
    }),
  ])
}
