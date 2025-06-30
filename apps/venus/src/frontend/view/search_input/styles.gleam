import sketch/css
import sketch/css/length.{px}
import sketch/css/media
import sketch/lustre/element/html as h

pub fn search_with_filters(attributes, children) {
  css.class([
    css.grid_area("input"),
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(12)),
  ])
  |> h.div(attributes, children)
}

pub fn search_input_wrapper(loading: Bool, children) {
  css.class([
    css.border_radius(px(12)),
    css.overflow("hidden"),
    css.padding(
      px(case loading {
        True -> 4
        False -> 0
      }),
    ),
    css.background(
      "linear-gradient(-45deg, #4ce7ff, #c651e5, #e3d8be, #4ce7ff)",
    ),
    css.property("background-size", "400% 400%"),
    css.transition("padding .3s"),
    css.animation("bg-spin 3s linear infinite"),
    css.border("1px solid var(--border-color)"),
    css.animation_play_state(case loading {
      True -> "running"
      False -> "paused"
    }),
  ])
  |> h.div([], children)
}

pub fn search_input(loading, small, children) {
  css.class([
    css.display("flex"),
    css.gap(px(6)),
    css.border_radius(px(8)),
    css.color("var(--input-text-color)"),
    css.background(case small {
      False -> "var(--input-background)"
      True -> "var(--dark-background)"
    }),
    css.transition("padding .3s"),
    css.align_items("center"),
    css.padding(
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
  css.class([
    css.appearance("none"),
    css.border("none"),
    css.outline("none"),
    css.width(length.percent(100)),
    css.line_height("1.5"),
    css.background("transparent"),
    css.color("inherit"),
  ])
  |> h.input(attributes)
}

pub fn shortcut_hint(attrs, children) {
  css.class([
    css.white_space("nowrap"),
    css.font_size(px(11)),
    css.border("1px solid var(--text-color)"),
    css.padding_("3px 6px"),
    css.border_radius(px(6)),
    css.opacity(0.4),
    css.media(media.max_width(px(700)), [css.display("none")]),
  ])
  |> h.div(attrs, children)
}
