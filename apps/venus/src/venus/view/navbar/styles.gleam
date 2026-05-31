import sketch/css
import sketch/css/length.{px, vw}
import sketch/css/media
import sketch/lustre/element/html as h
import venus/view/body/styles as body_styles

pub const search_lucy = body_styles.search_lucy

pub fn search_input_wrapper(attributes, children) {
  css.class([css.width_("100%")])
  |> h.form(attributes, children)
}

pub fn navbar_search_title(attributes, children) {
  css.class([
    css.font_size(length.rem(1.2)),
    css.compose(body_styles.search_title_()),
    css.text_decoration("none"),
    css.cursor("pointer"),
  ])
  |> h.a(attributes, children)
}

pub fn nav_links(attributes, children) {
  css.class([
    css.display("flex"),
    css.align_items("baseline"),
    css.gap(px(48)),
    css.padding(px(48)),
    css.media(media.max_width(px(700)), [css.padding(px(12)), css.gap(px(24))]),
  ])
  |> h.div(attributes, children)
}

pub fn coming_soon(attributes, children) {
  css.class([css.font_size(length.rem(0.7))])
  |> h.span(attributes, children)
}

pub fn trending(attributes, children) {
  css.class([
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(3)),
    css.align_items("end"),
    css.color("var(--text-color)"),
    css.white_space("nowrap"),
    css.opacity(0.3),
  ])
  |> h.div(attributes, children)
}

pub fn nav_link(attributes, children) {
  css.class([css.color("var(--text-color)"), css.text_decoration("none")])
  |> h.a(attributes, children)
}

pub fn navbar(transparent: Bool, attributes, children) {
  css.class([
    css.position("sticky"),
    css.top(px(0)),
    css.justify_content("end"),
    css.grid_area("navbar"),
    css.padding_left(px(48)),
    css.gap(px(48)),
    css.height(px(130)),
    css.z_index(1000),
    css.background(case transparent {
      True -> "transparent"
      False -> "var(--sidebar-background)"
    }),
    css.display(case transparent {
      True -> "flex"
      False -> "none"
    }),
    css.border_bottom(
      "1px solid "
      <> case transparent {
        True -> "transparent"
        False -> "var(--border-color)"
      },
    ),
    css.media(media.max_width(px(700)), [
      css.display("flex"),
      css.gap(px(24)),
      css.max_width(vw(100)),
      css.height_("unset"),
      css.padding_("18px 24px"),
    ]),
  ])
  |> h.nav(attributes, children)
}

pub fn navbar_search(attributes, children) {
  css.class([
    css.display("flex"),
    css.gap(px(48)),
    css.align_items("center"),
    css.flex("1"),
    css.media(media.max_width(px(700)), [css.gap(px(24))]),
  ])
  |> h.div(attributes, children)
}

pub fn title(a, c) {
  h.div_(a, c)
}
