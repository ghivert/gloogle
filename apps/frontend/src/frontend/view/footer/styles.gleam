import frontend/colors/palette
import sketch as s
import sketch/magic/element/html as h
import sketch/size.{px}

pub fn footer(attributes, children) {
  s.class([
    s.background("var(--sidebar-background)"),
    s.display("flex"),
    s.flex_direction("column"),
    s.padding(px(24)),
    s.align_items("center"),
    s.gap(px(48)),
    s.margin_top(px(48)),
    s.grid_area("footer"),
  ])
  |> h.footer(attributes, children)
}

pub fn footer_built(attributes, children) {
  s.class([
    s.align_items("center"),
    s.justify_content("center"),
    s.font_size(size.rem(0.8)),
    s.line_height("1.3"),
    s.text_align("center"),
  ])
  |> h.div(attributes, children)
}

pub fn footer_subtitles(attributes, children) {
  s.class([s.display("flex"), s.flex_direction("column"), s.gap(px(4))])
  |> h.div(attributes, children)
}

pub fn footer_links(attributes, children) {
  s.class([
    s.display("grid"),
    s.grid_template_columns("repeat(3, 1fr)"),
    s.grid_template_rows("repeat(6, auto)"),
    s.gap(px(12)),
    s.max_width(px(700)),
    s.width(size.percent(100)),
  ])
  |> h.div(attributes, children)
}

pub fn footer_section(attributes, children) {
  s.class([
    s.display("grid"),
    s.grid_template_columns("1fr"),
    s.grid_template_rows("subgrid"),
    s.grid_row("1 / 7"),
  ])
  |> h.div(attributes, children)
}

pub fn foot_title(attributes, children) {
  s.class([
    s.color("var(--input-text-color)"),
    s.font_weight("500"),
    s.padding_("6px 0px"),
  ])
  |> h.div(attributes, children)
}

pub fn foot_lk(attributes, children) {
  s.class([
    s.font_size(size.rem(0.9)),
    s.color(palette.dark.dark_white),
    s.text_decoration("none"),
  ])
  |> h.a(attributes, children)
}
