import frontend/colors/palette
import sketch as s
import sketch/size.{px, vh}

pub fn layout() {
  s.class([
    s.display("grid"),
    s.grid_template_areas(
      "\"navbar\"
       \"main\"
       \"footer\"",
    ),
    s.property("--a-color", palette.dark.faff_pink),
    s.grid_template_rows("auto 1fr auto"),
    s.min_height(vh(100)),
    s.background(palette.dark.underwater_blue),
    s.color(palette.dark.white),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn navbar() {
  s.class([
    s.position("sticky"),
    s.top(px(0)),
    s.display("flex"),
    s.align_items("baseline"),
    s.justify_content("end"),
    s.grid_area("navbar"),
    s.padding(px(48)),
    s.gap(px(48)),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn coming_soon() {
  s.class([s.font_size(size.rem_(0.7))])
  |> s.memo()
  |> s.to_lustre()
}

pub fn trending() {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(3)),
    s.align_items("end"),
    s.color(palette.dark.dark_white),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn search_wrapper() {
  s.class([
    s.display("grid"),
    s.grid_template_rows("auto auto auto"),
    s.grid_template_columns("auto auto auto"),
    s.grid_template_areas(
      "\"title .     .\"
       \"input input input\"
       \".     .     submit\"",
    ),
    s.padding(px(48)),
    s.gap(px(24)),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn main_wrapper() {
  s.class([
    s.grid_area("main"),
    s.display("flex"),
    s.flex_direction("column"),
    s.justify_content("center"),
    s.max_width(px(700)),
    s.width(size.percent(100)),
    s.margin_("auto"),
    s.gap(px(24)),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn search_title() {
  s.class([
    s.font_family("Lexend"),
    s.display("flex"),
    s.align_items("center"),
    s.gap(px(12)),
    s.font_size(size.rem_(2.5)),
    s.color(palette.dark.white),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn search_title_wrapper() {
  s.class([
    s.grid_area("title"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(9)),
    s.font_size(size.rem_(0.9)),
    s.color(palette.dark.dark_white),
    s.line_height("1.3"),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn search_input() {
  s.class([
    s.grid_area("input"),
    s.appearance("none"),
    s.background(palette.dark.white),
    s.border("none"),
    s.padding(px(18)),
    s.border_radius(px(18)),
    s.transition("outline .3s"),
    s.outline("2px solid transparent"),
    s.color(palette.dark.charcoal),
    s.active([s.outline("2px solid " <> palette.dark.faff_pink)]),
    s.focus([s.outline("2px solid " <> palette.dark.faff_pink)]),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn search_submit() {
  s.class([
    s.grid_area("submit"),
    s.appearance("none"),
    s.border("none"),
    s.background(palette.dark.faff_pink),
    s.border_radius(px(50)),
    s.padding_top(px(12)),
    s.padding_bottom(px(12)),
    s.padding_right(px(24)),
    s.padding_left(px(24)),
    s.color(palette.dark.charcoal),
    s.font_size(size.rem(1)),
    s.outline("none"),
    s.transition("background .3s"),
    s.active([s.background(palette.dark.dark_faff_pink)]),
    s.focus([s.background(palette.dark.dark_faff_pink)]),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn search_lucy() {
  s.class([s.width(px(40))])
  |> s.memo()
  |> s.to_lustre()
}

pub fn footer() {
  s.class([
    s.background(palette.dark.charcoal),
    s.display("flex"),
    s.flex_direction("column"),
    s.padding(px(24)),
    s.align_items("center"),
    s.gap(px(48)),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn footer_built() {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.align_items("center"),
    s.justify_content("center"),
    s.font_size(size.rem_(0.8)),
    s.line_height("1.3"),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn footer_links() {
  s.class([
    s.display("grid"),
    s.grid_template_columns("repeat(3, 1fr)"),
    s.grid_template_rows("repeat(6, auto)"),
    s.gap(px(12)),
    s.max_width(px(700)),
    s.width(size.percent(100)),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn footer_section() {
  s.class([
    s.display("grid"),
    s.grid_template_columns("1fr"),
    s.grid_template_rows("subgrid"),
    s.grid_row("1 / 7"),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn foot_title() {
  s.class([
    s.color(palette.dark.dark_white),
    s.font_weight("500"),
    s.padding_("6px 0px"),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn foot_lk() {
  s.class([s.font_size(size.rem_(0.9))])
  |> s.memo()
  |> s.to_lustre()
}

pub fn flex() {
  s.class([s.display("flex")])
  |> s.memo()
  |> s.to_lustre()
}

pub fn search_body() {
  s.class([
    s.background(palette.dark.black),
    s.border_radius(px(12)),
    s.margin(px(-12)),
    s.margin_top(px(12)),
    s.padding(px(12)),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn search_result() {
  s.class([
    s.background(palette.dark.unexpected_aubergine),
    s.border_radius(px(14)),
    s.overflow("hidden"),
    s.padding(px(12)),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn search_details() {
  s.class([
    s.background(palette.dark.unexpected_aubergine),
    s.display("flex"),
    s.gap(px(12)),
    s.justify_content("space-between"),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn signature() {
  s.class([s.white_space("pre-wrap"), s.display("block"), s.line_height("1.6")])
  |> s.memo()
  |> s.to_lustre()
}

pub fn documentation() {
  s.class([
    s.background(palette.dark.charcoal),
    s.padding(px(12)),
    s.border_radius(px(10)),
    s.margin(px(-12)),
    s.margin_top(px(12)),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(6)),
  ])
  |> s.memo()
  |> s.to_lustre()
}

pub fn documentation_title() {
  s.class([s.color(palette.dark.dark_white)])
  |> s.memo()
  |> s.to_lustre()
}
