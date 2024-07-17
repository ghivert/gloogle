import frontend/colors/palette
import gleam/bool
import gleam/int
import gleam/list
import lustre/attribute as a
import lustre/element
import sketch as s
import sketch/lustre/element as el
import sketch/media
import sketch/size.{percent, px, vh, vw}

pub fn implementations_pill_container(attrs, children) {
  let id = "implementations_pill_container"
  el.memo_dynamic("div", attrs, children, id, [
    s.display("flex"),
    s.align_items("center"),
    s.font_size(px(14)),
    s.font_weight("300"),
    s.gap(px(6)),
  ])
}

pub fn implementations_pill(background, color, attributes, children) {
  let id = "implementations-pill-" <> background
  s.dynamic(id, [
    s.background(background),
    s.border_radius(px(6)),
    s.width(px(8)),
    s.height(px(8)),
  ])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn implementations_pill_wrapper(attributes, children) {
  let id = "implementations_pill_wrapper"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("flex"),
    s.align_items("center"),
    s.gap(px(12)),
    s.justify_content("end"),
  ])
}

pub fn search_result(attributes, children) {
  let id = "search_result"
  el.memo_dynamic("div", attributes, children, id, [
    s.overflow("hidden"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(12)),
  ])
}

pub fn search_results_wrapper(attributes, children) {
  let id = "search_results_wrapper"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("grid"),
    s.padding_right(px(48)),
    s.gap(px(36)),
    s.grid_template_columns("min-content 1fr"),
    s.justify_items("center"),
    s.media(media.max_width(px(700)), [
      s.grid_template_columns("1fr"),
      s.padding_("0 24px"),
    ]),
  ])
}

pub fn external_icon_wrapper(attrs, children) {
  let id = "external_icon_wrapper"
  el.memo_dynamic("div", attrs, children, id, [
    s.width(px(16)),
    s.height(px(16)),
  ])
}

pub fn search_details(attributes, children) {
  let id = "search_details"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("flex"),
    s.gap(px(12)),
    s.align_items("center"),
    s.media(media.max_width(px(700)), [s.flex_direction("column")]),
  ])
}

pub fn search_details_title(attributes, children) {
  let id = "search_details_title"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("flex"),
    s.align_items("center"),
    s.gap(px(12)),
  ])
}

pub fn qualified_name(attributes, children) {
  let id = "qualified_name"
  el.memo_dynamic("a", attributes, children, id, [
    s.overflow("hidden"),
    s.text_overflow("ellipsis"),
    s.direction("rtl"),
    s.text_decoration("none"),
    s.hover([s.text_decoration("underline")]),
  ])
}

pub fn search_body(attributes, children) {
  let id = "search_body"
  el.memo_dynamic("div", attributes, children, id, [
    s.background("rgba(254, 254, 252, 0.05)"),
    s.border_radius(px(12)),
    s.padding_("12px 24px"),
    s.border("1px solid rgba(254, 254, 252, .1)"),
  ])
}

pub fn signature(attributes, children) {
  let id = "signature"
  el.memo_dynamic("code", attributes, children, id, [
    s.white_space("pre-wrap"),
    s.display("block"),
    s.line_height("1.6"),
    s.overflow("auto"),
  ])
}

pub fn documentation(attributes, children) {
  let id = "documentation"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(12)),
    s.padding_top(px(6)),
  ])
}

pub fn search_result_separator() {
  let id = "search_result_separator"
  el.memo_dynamic("div", [], [], id, [
    s.height(px(1)),
    s.background("rgba(254, 254, 252, 0.1)"),
    s.margin_("6px 0"),
  ])
}

pub fn documentation_title(attributes, children) {
  let id = "documentation_title"
  el.memo_dynamic("div", attributes, children, id, [
    s.color(palette.dark.dark_white),
  ])
}

pub fn search_wrapper(attributes, children) {
  let id = "search_wrapper"
  el.memo_dynamic("form", attributes, children, id, [
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
    s.media(media.max_width(px(700)), [
      s.max_width(vw(100)),
      s.padding(px(24)),
      s.grid_template_areas(
        "\"title title title\"
         \"input input input\"
         \".     .     submit\"",
      ),
    ]),
  ])
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
  let id = "search_title"
  el.memo_dynamic("div", attributes, children, id, [
    s.compose(search_title_()),
    s.font_size(size.rem_(2.5)),
  ])
}

pub fn search_title_wrapper(attributes, children) {
  let id = "search_title_wrapper"
  el.memo_dynamic("div", attributes, children, id, [
    s.grid_area("title"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(9)),
    s.font_size(size.rem_(0.9)),
    s.color(palette.dark.dark_white),
    s.line_height("1.3"),
  ])
}

pub fn search_lucy(size, attributes) {
  let id = "search-lucy-" <> int.to_string(size)
  s.dynamic(id, [s.width(px(size))])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("img", _, [])
}

pub fn search_submit(attributes) {
  let id = "search_submit"
  el.memo_dynamic("input", attributes, [], id, [
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
    s.disabled([s.background(palette.dark.unexpected_aubergine)]),
  ])
}

pub fn matches_titles(attributes, children) {
  let id = "matches_titles"
  el.memo_dynamic("div", attributes, children, id, [
    s.line_height("1.3"),
    s.color(palette.dark.dark_white),
    s.display("flex"),
    s.align_items("baseline"),
    s.gap(px(6)),
    s.font_size(px(12)),
    s.media(media.max_width(px(700)), [s.flex_direction("column")]),
  ])
}

pub fn matches_title(attributes, children) {
  let id = "matches_title"
  el.memo_dynamic("div", attributes, children, id, [
    s.color(palette.dark.white),
    s.font_size(px(18)),
  ])
}

pub fn empty_state(attributes, children) {
  let id = "empty_state"
  el.memo_dynamic("div", attributes, children, id, [
    s.grid_row("span 3"),
    s.display("flex"),
    s.align_items("center"),
    s.gap(px(24)),
    s.justify_content("center"),
    s.max_width(px(700)),
    s.width(size.percent(100)),
    s.margin_("auto"),
    s.padding(px(24)),
  ])
}

pub fn empty_state_lucy(attributes) {
  let id = "empty_state_lucy"
  el.memo_dynamic("img", attributes, [], id, [s.width(px(100))])
}

pub fn empty_state_titles(attributes, children) {
  let id = "empty_state_titles"
  el.memo_dynamic("div", attributes, children, id, [
    s.font_size(px(20)),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(9)),
    s.line_height("1.3"),
    s.max_width(px(400)),
  ])
}

pub fn empty_state_subtitle(attributes, children) {
  let id = "empty_state_subtitle"
  el.memo_dynamic("div", attributes, children, id, [
    s.font_size(px(16)),
    s.color(palette.dark.dark_white),
  ])
}

pub fn sidebar_wrapper(attributes, children) {
  let id = "sidebar_wrapper"
  el.memo_dynamic("div", attributes, children, id, [
    s.position("sticky"),
    s.top(px(0)),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(12)),
    s.height(vh(100)),
    s.overflow("auto"),
    s.padding_("12px"),
    s.border_right("1px solid rgba(254, 254, 252, .1)"),
    s.property("mask", {
      "linear-gradient(
        180deg,
        rgba(255,255,255, 0) 0%,
        rgba(255,255,255,1) 0.5% 99.5%,
        rgba(255, 255, 255, 0)
      )"
    }),
    s.media(media.max_width(px(700)), [s.display("none")]),
  ])
}

pub fn sidebar_wrapper_title(attrs, children) {
  let id = "sidebar_wrapper_title"
  el.memo_dynamic("div", attrs, children, id, [
    s.padding_("4px 4px"),
    s.color("#ffffff99"),
  ])
}

pub fn sidebar_package_name(attributes, children) {
  let id = "sidebar_package_name"
  el.memo_dynamic("div", attributes, children, id, [
    s.padding_left(px(8)),
    s.border_radius(px(6)),
    s.overflow("hidden"),
    s.text_overflow("ellipsis"),
  ])
}

pub fn sidebar_module_name(attributes, children) {
  let id = "sidebar_module_name"
  el.memo_dynamic("button", attributes, children, id, [
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
    s.padding_left(px(16)),
  ])
}

pub fn sidebar_package_wrapper(attributes, children) {
  let id = "sidebar_package_wrapper"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(9)),
    s.font_size(px(14)),
    s.max_width(px(250)),
  ])
}

pub fn main(attributes, children) {
  let id = "main"
  el.memo_dynamic("main", attributes, children, id, [
    s.grid_area("main"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(24)),
  ])
}

pub fn search_sidebar(attributes, children) {
  let id = "search_sidebar"
  el.memo_dynamic("main", attributes, children, id, [
    s.grid_area("sidebar"),
    s.display("flex"),
    s.flex_direction("column"),
    s.padding(px(16)),
    s.border_right("1px solid #ffffff1a"),
    s.background("#ffffff0d"),
    s.width(px(320)),
    s.gap(px(16)),
    s.height(vh(100)),
    s.position("sticky"),
    s.top(px(0)),
  ])
}

pub fn sidebar_title(attrs, children) {
  let id = "sidebar_title"
  el.memo_dynamic("a", attrs, children, id, [
    s.display("flex"),
    s.align_items("center"),
    s.gap(px(16)),
    s.color("inherit"),
    s.text_decoration("none"),
  ])
}

pub fn sidebar_title_inside(attrs, children) {
  let id = "sidebar_title_inside"
  el.memo_dynamic("div", attrs, children, id, [])
}

pub fn form_wrapper(attrs, children) {
  let id = "form_wrapper"
  el.memo_dynamic("form", attrs, children, id, [])
}

pub fn sidebar_filter(attrs, children) {
  let id = "sidebar_filter"
  el.memo_dynamic("div", attrs, children, id, [
    s.padding_top(px(12)),
    s.padding_left(px(12)),
    s.color("rgba(254, 254, 252, .6)"),
  ])
}

pub fn sidebar_filters(attrs, children) {
  let id = "sidebar_filters"
  el.memo_dynamic("div", attrs, children, id, [
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(18)),
    s.padding_("0 12px"),
  ])
}

pub fn sidebar_checkbox(active, attrs) {
  let val = bool.to_string(active)
  let id1 = "sidebar-checkbox-div-" <> val
  let id2 = "sidebar-checkbox-input-" <> val
  element.fragment([
    el.memo_dynamic("div", [], [], id1, [
      s.width(px(16)),
      s.height(px(16)),
      s.background(case active {
        True -> "#ffaff3"
        False -> "rgba(254, 254, 252, .1)"
      }),
      s.border("1px solid rgba(254, 254, 252, .1)"),
      s.border_radius(px(4)),
    ]),
    el.memo_dynamic(
      "input",
      [a.type_("checkbox"), a.checked(active), ..attrs],
      [],
      id2,
      [s.position("fixed"), s.top(px(-1000)), s.width(px(1)), s.height(px(1))],
    ),
  ])
}

pub fn sidebar_spacer(attrs, children) {
  let id = "sidebar_spacer"
  el.memo_dynamic("div", attrs, children, id, [s.flex("1")])
}

pub fn filter_separator(attrs, children) {
  let id = "filter_separator"
  el.memo_dynamic("div", attrs, children, id, [
    s.height(px(1)),
    s.background("rgba(254, 254, 252, .1)"),
  ])
}

pub fn sidebar_filter_line(attrs, children) {
  let id = "sidebar_filter_line"
  el.memo_dynamic("label", attrs, children, id, [
    s.display("flex"),
    s.gap(px(9)),
    s.cursor("pointer"),
    s.align_items("center"),
  ])
}

pub fn sidebar_filter_name(attrs, children) {
  let id = "sidebar_filter_name"
  el.memo_dynamic("div", attrs, children, id, [s.color("white")])
}

pub fn sidebar_links(attrs, children) {
  let id = "sidebar_links"
  el.memo_dynamic("div", attrs, children, id, [
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(24)),
    s.padding(px(12)),
  ])
}

pub fn sidebar_link_wrapper(attrs, children) {
  let id = "sidebar_link_wrapper"
  el.memo_dynamic("a", attrs, children, id, [
    s.display("flex"),
    s.align_items("baseline"),
    s.gap(px(9)),
    s.text_decoration("none"),
    s.color("inherit"),
  ])
}

pub fn sidebar_icon(attrs, children) {
  let id = "sidebar_icon"
  el.memo_dynamic("div", attrs, children, id, [
    s.width(px(12)),
    s.height(px(12)),
  ])
}

pub fn sidebar_link(attrs, children) {
  let id = "sidebar_link"
  el.memo_dynamic("div", attrs, children, id, [
    s.font_size(px(14)),
    s.color("rgba(254, 254, 252, 0.6)"),
  ])
}

pub fn items_wrapper(attributes, children) {
  let id = "items_wrapper"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(24)),
    s.padding_top(px(24)),
    s.max_width(px(700)),
    s.width(size.percent(100)),
    s.overflow("hidden"),
  ])
}

pub fn named_type_button(attributes, children) {
  let id = "named_type_button"
  el.memo_dynamic("a", attributes, children, id, [s.color("#e5c07b")])
}

pub fn search_title_with_hint(attributes, children) {
  let id = "search_title_with_hint"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("flex"),
    s.gap(px(12)),
  ])
}

pub fn pre_alpha_title(attributes, children) {
  let id = "pre_alpha_title"
  el.memo_dynamic("div", attributes, children, id, [s.font_size(px(16))])
}

pub fn loading_trending(attributes, children) {
  let id = "loading_trending"
  el.memo_dynamic("div", attributes, children, id, [])
}

pub fn no_trendings(attributes, children) {
  let id = "no_trendings"
  el.memo_dynamic("div", attributes, children, id, [])
}

pub fn trendings_wrapper(attributes, children) {
  let id = "trendings_wrapper"
  el.memo_dynamic("div", attributes, children, id, [s.padding_("12px 48px")])
}

pub fn trendings_title(attributes, children) {
  let id = "trendings_title"
  el.memo_dynamic("div", attributes, children, id, [s.margin_bottom(px(24))])
}

pub fn trendings_grid(attributes, children) {
  let id = "trendings_grid"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("grid"),
    // s.grid_template_columns("repeat(auto-fill, minmax(350px, 1fr))"),
    s.align_items("start"),
    s.gap(px(24)),
  ])
}

pub fn trendings_card(attributes, children) {
  let id = "trendings_card"
  el.memo_dynamic("div", attributes, children, id, [
    s.background(palette.dark.unexpected_aubergine),
  ])
}

pub fn documentation_links(attributes, children) {
  let id = "documentation_links"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("flex"),
    s.justify_content("space-between"),
  ])
}

pub fn licenses(attributes, children) {
  let id = "licenses"
  el.memo_dynamic("div", attributes, children, id, [
    s.display("flex"),
    s.gap(px(6)),
  ])
}
