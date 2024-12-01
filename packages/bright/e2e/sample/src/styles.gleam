import gleam/int
import icons
import lustre/attribute as a
import sketch
import sketch/lustre/element/html as h
import sketch/media
import sketch/size.{px}

pub fn intro() {
  sketch.class([
    sketch.padding(px(40)),
    sketch.padding_top(px(0)),
    sketch.margin_bottom(px(20)),
    sketch.first_of_type([sketch.padding_top(px(40))]),
  ])
}

pub fn title(title) {
  sketch.class([sketch.font_weight("bold")])
  |> h.h2([], [h.text(title)])
}

pub fn nav() {
  sketch.class([
    sketch.font_size(size.rem(1.3)),
    sketch.font_weight("bold"),
    sketch.display("flex"),
    sketch.justify_content("space-between"),
    sketch.margin(px(18)),
    sketch.gap(px(36)),
    sketch.background("var(--navbar-background)"),
    sketch.position("sticky"),
    sketch.border_radius(px(10)),
    sketch.top(px(18)),
    sketch.border("1px solid var(--dark-background)"),
    sketch.backdrop_filter("blur(8px)"),
  ])
  |> h.nav([a.id("navbar")], [
    sketch.class([
      sketch.display("flex"),
      sketch.align_items("center"),
      sketch.padding_left(px(18)),
    ])
      |> h.div([], [h.text("Bright")]),
    h.div_([], []),
    h.div(
      sketch.class([
        sketch.display("flex"),
        sketch.gap(px(24)),
        sketch.padding(px(18)),
      ]),
      [],
      [
        external_icon("https://hexdocs.pm/bright", icons.book_open()),
        external_icon("https://github.com/ghivert/bright", icons.github()),
      ],
    ),
  ])
}

fn external_icon(url, icon) {
  sketch.class([
    sketch.color("#aaa"),
    sketch.transition("all .3s"),
    sketch.hover([sketch.color("var(--text-color)")]),
  ])
  |> h.a([a.href(url)], [icons.small(icon)])
}

pub fn counter(attrs, children) {
  sketch.class([
    sketch.display("flex"),
    sketch.flex_direction("column"),
    sketch.align_items("center"),
    sketch.background("var(--darker-background)"),
    sketch.color("var(--text-color)"),
    sketch.height(px(220)),
    sketch.width(px(220)),
    sketch.border_radius(px(2)),
    sketch.position("relative"),
    sketch.z_index(100),
    sketch.border_radius(px(10)),
  ])
  |> h.div(attrs, children)
}

pub fn button(attrs, children) {
  sketch.class([
    sketch.appearance("none"),
    sketch.border_radius(px(5)),
    sketch.background("var(--dark-background)"),
    sketch.display("flex"),
    sketch.border("1px solid var(--border-color)"),
    sketch.align_items("center"),
    sketch.justify_content("center"),
    sketch.padding(px(10)),
    sketch.cursor("pointer"),
    sketch.font_family("inherit"),
    sketch.color("var(--text-color)"),
    sketch.font_size_("inherit"),
    sketch.text_transform("uppercase"),
    sketch.font_weight("bold"),
    sketch.hover([sketch.background("var(--button-hover)")]),
  ])
  |> h.button(attrs, children)
}

pub fn counter_number(counter) {
  sketch.class([
    sketch.flex("1"),
    sketch.display("flex"),
    sketch.align_items("center"),
    sketch.padding_top(px(20)),
    sketch.font_weight("bold"),
    sketch.font_size(size.rem(1.4)),
  ])
  |> h.div([], [h.text(int.to_string(counter))])
}

pub fn buttons_wrapper(attrs, children) {
  sketch.class([
    sketch.display("flex"),
    sketch.flex_direction("column"),
    sketch.padding(px(10)),
    sketch.justify_content("space-evenly"),
    sketch.width(size.percent(100)),
    sketch.gap(px(10)),
  ])
  |> h.div(attrs, children)
}

pub fn counter_infos(attrs, children) {
  sketch.class([
    sketch.font_family("Fira Code"),
    sketch.background("var(--dark-background)"),
    sketch.position("absolute"),
    sketch.top(px(110)),
    sketch.left(px(50)),
    sketch.width(px(250)),
    sketch.height(px(250)),
    sketch.z_index(10),
    sketch.display("flex"),
    sketch.flex_direction("column"),
    sketch.justify_content("end"),
    sketch.padding(px(10)),
    sketch.border_radius(px(10)),
    sketch.media(media.max_width(px(400)), [sketch.width(px(200))]),
  ])
  |> h.div(attrs, children)
}

pub fn computed(title, content) {
  h.div_([], [h.text(title), h.text(int.to_string(content))])
}

pub fn container(attrs, children) {
  sketch.class([
    sketch.display("flex"),
    sketch.gap(px(10)),
    sketch.justify_content("center"),
    sketch.media(media.max_width(px(700)), [
      sketch.flex_direction("column"),
      sketch.align_items("center"),
    ]),
  ])
  |> h.div(attrs, children)
}

pub fn counter_wrapper(attrs, children) {
  sketch.class([
    sketch.position("relative"),
    sketch.width(px(350)),
    sketch.height(px(400)),
    sketch.media(media.max_width(px(400)), [sketch.width(px(250))]),
  ])
  |> h.div(attrs, children)
}

pub fn footer(attrs, children) {
  sketch.class([
    sketch.text_align("center"),
    sketch.margin_top(px(60)),
    sketch.margin_bottom(px(30)),
    sketch.color("var(--text-grey)"),
  ])
  |> h.div(attrs, children)
}

pub fn body(attrs, children) {
  sketch.class([sketch.max_width(px(1000)), sketch.margin_("auto")])
  |> h.div(attrs, children)
}
