import frontend/colors/palette
import gleam/bool
import sketch as s
import sketch/lustre/element
import sketch/media
import sketch/size.{px}

pub fn search_with_filters(attributes, children) {
  element.element("div", attributes, children, [
    s.grid_area("input"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(12)),
  ])
}

pub fn search_input_wrapper(loading: Bool, children) {
  element.element("div", [], children, [
    s.border_radius(px(18)),
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
    s.animation_play_state(case loading {
      True -> "running"
      False -> "paused"
    }),
  ])
}

pub fn search_input(loading, children) {
  element.element("div", [], children, [
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
  element.element("input", attributes, [], [
    s.appearance("none"),
    s.border("none"),
    s.outline("none"),
    s.width(size.percent(100)),
    s.line_height("1.5"),
  ])
}

pub fn filter_pills(attributes, children) {
  element.element("div", attributes, children, [
    s.display("flex"),
    s.gap(px(12)),
    s.justify_content("end"),
    s.media(media.max_width(px(700)), [s.display("none")]),
  ])
}

pub fn filter_pill(active: Bool, attributes, children) {
  element.element("button", attributes, children, [
    s.background(palette.dark.aged_plastic_yellow),
    s.color(palette.dark.charcoal),
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
