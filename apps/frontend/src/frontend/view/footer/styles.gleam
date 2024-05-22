import frontend/colors/palette
import sketch as s
import sketch/lustre/extra as l
import sketch/size.{px}

pub fn footer(attributes, children) {
  l.memo("footer", attributes, children, [
    s.background(palette.dark.charcoal),
    s.display("flex"),
    s.flex_direction("column"),
    s.padding(px(24)),
    s.align_items("center"),
    s.gap(px(48)),
    s.margin_top(px(48)),
  ])
}

pub fn footer_built(attributes, children) {
  l.memo("div", attributes, children, [
    s.display("flex"),
    s.flex_direction("column"),
    s.align_items("center"),
    s.justify_content("center"),
    s.font_size(size.rem_(0.8)),
    s.line_height("1.3"),
  ])
}

pub fn footer_links(attributes, children) {
  l.memo("div", attributes, children, [
    s.display("grid"),
    s.grid_template_columns("repeat(3, 1fr)"),
    s.grid_template_rows("repeat(6, auto)"),
    s.gap(px(12)),
    s.max_width(px(700)),
    s.width(size.percent(100)),
  ])
}

pub fn footer_section(attributes, children) {
  l.memo("div", attributes, children, [
    s.display("grid"),
    s.grid_template_columns("1fr"),
    s.grid_template_rows("subgrid"),
    s.grid_row("1 / 7"),
  ])
}

pub fn foot_title(attributes, children) {
  l.memo("div", attributes, children, [
    s.color(palette.dark.dark_white),
    s.font_weight("500"),
    s.padding_("6px 0px"),
  ])
}

pub fn foot_lk(attributes, children) {
  l.memo("a", attributes, children, [
    s.font_size(size.rem_(0.9)),
    s.color(palette.dark.white),
    s.text_decoration("none"),
  ])
}
