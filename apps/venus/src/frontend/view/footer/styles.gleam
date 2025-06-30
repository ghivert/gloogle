import frontend/colors/palette
import sketch/css
import sketch/css/length.{px}
import sketch/lustre/element/html as h

pub fn footer(attributes, children) {
  css.class([
    css.background("var(--sidebar-background)"),
    css.display("flex"),
    css.flex_direction("column"),
    css.padding(px(24)),
    css.align_items("center"),
    css.gap(px(48)),
    css.margin_top(px(48)),
    css.grid_area("footer"),
  ])
  |> h.footer(attributes, children)
}

pub fn footer_built(attributes, children) {
  css.class([
    css.align_items("center"),
    css.justify_content("center"),
    css.font_size(length.rem(0.8)),
    css.line_height("1.3"),
    css.text_align("center"),
  ])
  |> h.div(attributes, children)
}

pub fn footer_subtitles(attributes, children) {
  css.class([css.display("flex"), css.flex_direction("column"), css.gap(px(4))])
  |> h.div(attributes, children)
}

pub fn footer_links(attributes, children) {
  css.class([
    css.display("grid"),
    css.grid_template_columns("repeat(3, 1fr)"),
    css.grid_template_rows("repeat(6, auto)"),
    css.gap(px(12)),
    css.max_width(px(700)),
    css.width(length.percent(100)),
  ])
  |> h.div(attributes, children)
}

pub fn footer_section(attributes, children) {
  css.class([
    css.display("grid"),
    css.grid_template_columns("1fr"),
    css.grid_template_rows("subgrid"),
    css.grid_row("1 / 7"),
  ])
  |> h.div(attributes, children)
}

pub fn foot_title(attributes, children) {
  css.class([
    css.color("var(--input-text-color)"),
    css.font_weight("500"),
    css.padding_("6px 0px"),
  ])
  |> h.div(attributes, children)
}

pub fn foot_lk(attributes, children) {
  css.class([
    css.font_size(length.rem(0.9)),
    css.color(palette.dark.dark_white),
    css.text_decoration("none"),
  ])
  |> h.a(attributes, children)
}
