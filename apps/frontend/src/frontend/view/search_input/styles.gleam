import sketch as s
import sketch/magic/element/html as h
import sketch/media
import sketch/size.{px}

pub fn search_with_filters(attributes, children) {
  s.class([
    s.grid_area("input"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(12)),
  ])
  |> h.div(attributes, children)
}

pub fn search_input_wrapper(loading: Bool, children) {
  s.class([
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
  |> h.div([], children)
}

pub fn search_input(loading, small, children) {
  s.class([
    s.display("flex"),
    s.gap(px(6)),
    s.border_radius(px(8)),
    s.color("var(--input-text-color)"),
    s.background(case small {
      False -> "var(--input-background)"
      True -> "var(--dark-background)"
    }),
    s.transition("padding .3s"),
    s.align_items("center"),
    s.padding(
      px(case loading, small {
        True, False -> 16
        False, False -> 20
        True, True -> 6
        False, True -> 10
      }),
    ),
  ])
  |> h.div([], children)
}

pub fn search_input_content(attributes) {
  s.class([
    s.appearance("none"),
    s.border("none"),
    s.outline("none"),
    s.width(size.percent(100)),
    s.line_height("1.5"),
    s.background("transparent"),
    s.color("inherit"),
  ])
  |> h.input(attributes)
}

pub fn shortcut_hint(attrs, children) {
  s.class([
    s.white_space("nowrap"),
    s.font_size(px(11)),
    s.border("1px solid var(--text-color)"),
    s.padding_("3px 6px"),
    s.border_radius(px(6)),
    s.opacity(0.4),
    s.media(media.max_width(px(700)), [s.display("none")]),
  ])
  |> h.div(attrs, children)
}
