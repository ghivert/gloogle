import frontend/colors/palette
import frontend/view/body/styles as body_styles
import gleam/list
import lustre/element
import sketch as s
import sketch/size.{px}

pub const search_lucy = body_styles.search_lucy

pub const search_input = body_styles.search_input

pub fn search_input_wrapper(attributes, children) {
  s.class([s.width_("100%")])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("form", _, children)
}

pub fn navbar_search_title(attributes, children) {
  s.class([
    s.font_size(size.rem_(1.2)),
    s.compose(body_styles.search_title_()),
    s.text_decoration("none"),
    s.cursor("pointer"),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("a", _, children)
}

pub fn nav_links(attributes, children) {
  s.class([
    s.display("flex"),
    s.align_items("baseline"),
    s.gap(px(48)),
    s.padding(px(48)),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn coming_soon(attributes, children) {
  s.class([s.font_size(size.rem_(0.7))])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("span", _, children)
}

pub fn trending(attributes, children) {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(3)),
    s.align_items("end"),
    s.color(palette.dark.dark_white),
    s.white_space("nowrap"),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn navbar(attributes, children) {
  s.class([
    s.position("sticky"),
    s.top(px(0)),
    s.display("flex"),
    s.align_items("center"),
    s.justify_content("space-between"),
    s.grid_area("navbar"),
    s.padding_left(px(48)),
    s.gap(px(48)),
    s.background(palette.dark.underwater_blue),
    s.height(px(130)),
    s.z_index(1000),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn navbar_search(attributes, children) {
  s.class([
    s.display("flex"),
    s.gap(px(48)),
    s.align_items("center"),
    s.flex("1"),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}
