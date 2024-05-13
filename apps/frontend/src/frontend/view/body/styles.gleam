import frontend/colors/palette
import gleam/list
import lustre/element
import sketch as s
import sketch/size.{px}

pub fn implementations_pill(
  background: String,
  color: String,
  attributes,
  children,
) {
  let id = "implementations-pill-" <> background
  s.dynamic(id, [
    s.background(background),
    s.color(color),
    s.padding_("4px 9px"),
    s.border_radius(px(6)),
    s.font_size(px(10)),
  ])
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn implementations_pill_wrapper(attributes, children) {
  s.class([s.display("flex"), s.align_items("center"), s.gap(px(6))])
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn search_result(attributes, children) {
  s.class([
    s.background(palette.dark.unexpected_aubergine),
    s.border_radius(px(14)),
    s.overflow("hidden"),
    s.padding(px(12)),
    s.box_shadow("0 0 3px 2px " <> palette.dark.black <> "4d"),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn search_results_wrapper(attributes, children) {
  s.class([
    s.display("grid"),
    s.padding_("0 48px"),
    s.gap(px(24)),
    s.grid_template_columns("min-content minmax(auto, 1fr)"),
    s.justify_items("center"),
  ])
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn search_details(attributes, children) {
  s.class([
    s.background(palette.dark.unexpected_aubergine),
    s.display("flex"),
    s.gap(px(12)),
    s.justify_content("space-between"),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn search_details_title(attributes, children) {
  s.class([s.display("flex"), s.align_items("center"), s.gap(px(12))])
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn qualified_name(attributes, children) {
  s.class([
    s.overflow("hidden"),
    s.text_overflow("ellipsis"),
    s.direction("rtl"),
  ])
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn search_body(attributes, children) {
  s.class([
    s.background(palette.dark.black),
    s.border_radius(px(12)),
    s.margin(px(-12)),
    s.margin_top(px(12)),
    s.padding(px(12)),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn signature(attributes, children) {
  s.class([s.white_space("pre-wrap"), s.display("block"), s.line_height("1.6")])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("code", _, children)
}

pub fn documentation(attributes, children) {
  s.class([
    s.background(palette.dark.charcoal),
    s.padding(px(12)),
    s.border_radius(px(10)),
    s.margin(px(-12)),
    s.margin_top(px(12)),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(12)),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn documentation_title(attributes, children) {
  s.class([s.color(palette.dark.dark_white)])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn search_wrapper(attributes, children) {
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
    s.max_width(px(700)),
    s.width(size.percent(100)),
    s.margin_("auto"),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("form", _, children)
}

pub fn search_title_() {
  s.class([
    s.font_family("Lexend"),
    s.display("flex"),
    s.align_items("center"),
    s.gap(px(12)),
    s.color(palette.dark.white),
  ])
}

pub fn search_title(attributes, children) {
  s.class([s.compose(search_title_()), s.font_size(size.rem_(2.5))])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn search_title_wrapper(attributes, children) {
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
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn search_lucy(attributes) {
  s.class([s.width(px(40))])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("img", _, [])
}

pub fn search_input(attributes) {
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
    s.width(size.percent(100)),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("input", _, [])
}

pub fn search_submit(attributes) {
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
  |> list.prepend(attributes, _)
  |> element.element("input", _, [])
}

pub fn matches_titles(attributes, children) {
  s.class([
    s.line_height("1.3"),
    s.color(palette.dark.dark_white),
    s.display("flex"),
    s.align_items("baseline"),
    s.gap(px(6)),
    s.font_size(px(12)),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn matches_title(attributes, children) {
  s.class([s.color(palette.dark.white), s.font_size(px(18))])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn empty_state(attributes, children) {
  s.class([
    s.display("flex"),
    s.align_items("center"),
    s.gap(px(24)),
    s.justify_content("center"),
    s.max_width(px(700)),
    s.width(size.percent(100)),
    s.margin_("auto"),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn empty_state_lucy(attributes) {
  s.class([s.width(px(100))])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("img", _, [])
}

pub fn empty_state_titles(attributes, children) {
  s.class([
    s.font_size(px(20)),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(9)),
    s.line_height("1.3"),
    s.max_width(px(400)),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn empty_state_subtitle(attributes, children) {
  s.class([s.font_size(px(16)), s.color(palette.dark.dark_white)])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn sidebar_wrapper(attributes, children) {
  s.class([
    s.position("sticky"),
    s.top(px(130)),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(12)),
    s.height_("calc(100vh - 130px)"),
    s.overflow("auto"),
    s.padding_("12px 0"),
    s.property(
      "mask",
      "linear-gradient(180deg, rgba(255,255,255, 0) 0%, rgba(255,255,255,1) 1% 99%, rgba(255, 255, 255, 0))",
    ),
  ])
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn sidebar_package_name(attributes, children) {
  s.class([
    s.background(palette.dark.unexpected_aubergine),
    s.padding(px(6)),
    s.border_radius(px(6)),
    s.overflow("hidden"),
    s.text_overflow("ellipsis"),
  ])
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn sidebar_module_name(attributes, children) {
  s.class([
    s.text_overflow("ellipsis"),
    s.overflow("hidden"),
    s.direction("rtl"),
    s.text_align("left"),
    s.cursor("pointer"),
    s.hover([s.text_decoration("underline")]),
    s.appearance("none"),
    s.background("inherit"),
    s.border("none"),
    s.color("inherit"),
    s.font_size_("inherit"),
    s.line_height("inherit"),
    s.display("block"),
    s.property("padding-block", "0"),
    s.property("padding-inline", "0"),
    s.padding_left(px(12)),
  ])
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("button", _, children)
}

pub fn sidebar_package_wrapper(attributes, children) {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(9)),
    s.font_size(px(14)),
    s.max_width(px(250)),
  ])
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn main(attributes, children) {
  s.class([
    s.grid_area("main"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(24)),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("main", _, children)
}

pub fn items_wrapper(attributes, children) {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(24)),
    s.padding_top(px(12)),
    s.max_width(px(700)),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}
