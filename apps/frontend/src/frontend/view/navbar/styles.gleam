import frontend/view/body/styles as body_styles
import sketch as s
import sketch/magic/element/html as h
import sketch/media
import sketch/size.{px, vw}

pub const search_lucy = body_styles.search_lucy

pub fn search_input_wrapper(attributes, children) {
  s.class([s.width_("100%")])
  |> h.form(attributes, children)
}

pub fn navbar_search_title(attributes, children) {
  s.class([
    s.font_size(size.rem(1.2)),
    s.compose(body_styles.search_title_()),
    s.text_decoration("none"),
    s.cursor("pointer"),
  ])
  |> h.a(attributes, children)
}

pub fn nav_links(attributes, children) {
  s.class([
    s.display("flex"),
    s.align_items("baseline"),
    s.gap(px(48)),
    s.padding(px(48)),
    s.media(media.max_width(px(700)), [s.padding(px(12)), s.gap(px(24))]),
  ])
  |> h.div(attributes, children)
}

pub fn coming_soon(attributes, children) {
  s.class([s.font_size(size.rem(0.7))])
  |> h.span(attributes, children)
}

pub fn trending(attributes, children) {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(3)),
    s.align_items("end"),
    s.color("var(--text-color)"),
    s.white_space("nowrap"),
    s.opacity(0.3),
  ])
  |> h.div(attributes, children)
}

pub fn nav_link(attributes, children) {
  s.class([s.color("var(--text-color)"), s.text_decoration("none")])
  |> h.a(attributes, children)
}

pub fn navbar(transparent: Bool, attributes, children) {
  s.class([
    s.position("sticky"),
    s.top(px(0)),
    s.justify_content("end"),
    s.grid_area("navbar"),
    s.padding_left(px(48)),
    s.gap(px(48)),
    s.height(px(130)),
    s.z_index(1000),
    s.background(case transparent {
      True -> "transparent"
      False -> "var(--sidebar-background)"
    }),
    s.display(case transparent {
      True -> "flex"
      False -> "none"
    }),
    s.border_bottom(
      "1px solid "
      <> case transparent {
        True -> "transparent"
        False -> "var(--border-color)"
      },
    ),
    s.media(media.max_width(px(700)), [
      s.display("flex"),
      s.gap(px(24)),
      s.max_width(vw(100)),
      s.height_("unset"),
      s.padding_("18px 24px"),
    ]),
  ])
  |> h.nav(attributes, children)
}

pub fn navbar_search(attributes, children) {
  s.class([
    s.display("flex"),
    s.gap(px(48)),
    s.align_items("center"),
    s.flex("1"),
    s.media(media.max_width(px(700)), [s.gap(px(24))]),
  ])
  |> h.div(attributes, children)
}

pub fn title(a, c) {
  h.div_(a, c)
}
