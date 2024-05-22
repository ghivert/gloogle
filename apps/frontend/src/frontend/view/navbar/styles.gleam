import frontend/colors/palette
import frontend/view/body/styles as body_styles
import gleam/bool
import sketch as s
import sketch/lustre/extra as l
import sketch/media
import sketch/size.{px, vw}

pub const search_lucy = body_styles.search_lucy

pub fn search_input_wrapper(attributes, children) {
  l.memo("form", attributes, children, [s.width_("100%")])
}

pub fn navbar_search_title(attributes, children) {
  l.memo("a", attributes, children, [
    s.font_size(size.rem_(1.2)),
    s.compose(body_styles.search_title_()),
    s.text_decoration("none"),
    s.cursor("pointer"),
  ])
}

pub fn nav_links(attributes, children) {
  l.memo("div", attributes, children, [
    s.display("flex"),
    s.align_items("baseline"),
    s.gap(px(48)),
    s.padding(px(48)),
    s.media(media.max_width(px(700)), [s.display("none")]),
  ])
}

pub fn coming_soon(attributes, children) {
  l.memo("span", attributes, children, [s.font_size(size.rem_(0.7))])
}

pub fn trending(attributes, children) {
  l.memo("div", attributes, children, [
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(3)),
    s.align_items("end"),
    s.color(palette.dark.dark_white),
    s.white_space("nowrap"),
  ])
}

pub fn nav_link(attributes, children) {
  l.memo("a", attributes, children, [
    s.color(palette.dark.white),
    s.text_decoration("none"),
  ])
}

pub fn navbar(transparent: Bool, attributes, children) {
  let id = "navbar-transparent-" <> bool.to_string(transparent)
  l.dynamic("nav", attributes, children, id, [
    s.position("sticky"),
    s.top(px(0)),
    s.display("flex"),
    s.align_items("center"),
    s.justify_content("space-between"),
    s.grid_area("navbar"),
    s.padding_left(px(48)),
    s.gap(px(48)),
    s.background(case transparent {
      True -> "transparent"
      False -> palette.dark.underwater_blue
    }),
    s.height(px(130)),
    s.z_index(1000),
    s.media(media.max_width(px(700)), [
      s.padding_left(px(24)),
      s.padding_right(px(24)),
      s.gap(px(24)),
      s.max_width(vw(100)),
      s.height(px(115)),
    ]),
  ])
}

pub fn navbar_search(attributes, children) {
  l.memo("div", attributes, children, [
    s.display("flex"),
    s.gap(px(48)),
    s.align_items("center"),
    s.flex("1"),
    s.media(media.max_width(px(700)), [s.gap(px(24))]),
  ])
}

pub fn title(a, c) {
  l.memo("div", a, c, [s.media(media.max_width(px(700)), [s.display("none")])])
}
