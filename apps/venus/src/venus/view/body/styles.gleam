import lustre/attribute as a
import lustre/element
import sketch/css
import sketch/css/length.{px, vh, vw}
import sketch/css/media
import sketch/lustre/element/html as h
import venus/colors/palette

pub fn implementations_pill_container(attrs, children) {
  css.class([
    css.display("flex"),
    css.align_items("center"),
    css.font_size(px(14)),
    css.font_weight("300"),
    css.gap(px(6)),
  ])
  |> h.div(attrs, children)
}

pub fn implementations_pill(background, attributes, children) {
  css.class([
    css.background(background),
    css.border_radius(px(6)),
    css.width(px(8)),
    css.height(px(8)),
  ])
  |> h.div(attributes, children)
}

pub fn implementations_pill_wrapper(attributes, children) {
  css.class([
    css.display("flex"),
    css.align_items("center"),
    css.gap(px(12)),
    css.justify_content("end"),
  ])
  |> h.div(attributes, children)
}

pub fn search_result(attributes, children) {
  css.class([
    css.overflow("hidden"),
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(12)),
  ])
  |> h.div(attributes, children)
}

pub fn search_results_wrapper(attributes, children) {
  css.class([
    css.display("grid"),
    css.padding_right(px(48)),
    css.gap(px(36)),
    css.grid_template_columns("min-content 1fr"),
    css.justify_items("center"),
    css.media(media.max_width(px(700)), [
      css.grid_template_columns("1fr"),
      css.padding_("0 24px"),
    ]),
  ])
  |> h.div(attributes, children)
}

pub fn external_icon_wrapper(attrs, children) {
  css.class([css.width(px(16)), css.height(px(16))])
  |> h.div(attrs, children)
}

pub fn search_details(attributes, children) {
  css.class([
    css.display("flex"),
    css.gap(px(12)),
    css.align_items("center"),
    css.media(media.max_width(px(700)), [css.flex_direction("column")]),
  ])
  |> h.div(attributes, children)
}

pub fn search_details_title(attributes, children) {
  css.class([css.display("flex"), css.align_items("center"), css.gap(px(12))])
  |> h.div(attributes, children)
}

pub fn qualified_name(attributes, children) {
  css.class([
    css.overflow("hidden"),
    css.text_overflow("ellipsis"),
    css.text_decoration("none"),
    css.hover([css.text_decoration("underline")]),
  ])
  |> h.a(attributes, children)
}

pub fn search_body(attributes, children) {
  css.class([
    css.background("rgba(254, 254, 252, 0.05)"),
    css.border_radius(px(12)),
    css.padding_("12px 24px"),
    css.border("1px solid rgba(254, 254, 252, .1)"),
  ])
  |> h.div(attributes, children)
}

pub fn signature(attributes, children) {
  css.class([
    css.white_space("pre-wrap"),
    css.display("block"),
    css.line_height("1.6"),
    css.overflow("auto"),
  ])
  |> h.code(attributes, children)
}

pub fn documentation(attributes, children) {
  css.class([
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(12)),
    css.padding_top(px(6)),
  ])
  |> h.div(attributes, children)
}

pub fn search_result_separator() {
  css.class([
    css.height(px(1)),
    css.background("rgba(254, 254, 252, 0.1)"),
    css.margin_("6px 0"),
  ])
  |> h.div([], [])
}

pub fn documentation_title(attributes, children) {
  css.class([css.color(palette.dark.dark_white)])
  |> h.div(attributes, children)
}

pub fn search_wrapper(attributes, children) {
  css.class([
    css.display("grid"),
    css.grid_template_rows("auto auto auto"),
    css.grid_template_columns("auto auto auto"),
    css.grid_template_areas(["title . .", "input input input", ". . submit"]),
    css.padding(px(48)),
    css.gap(px(24)),
    css.max_width(px(700)),
    css.width(length.percent(100)),
    css.margin_("auto"),
    css.media(media.max_width(px(700)), [
      css.max_width(vw(100)),
      css.padding(px(24)),
      css.grid_template_areas([
        "title title title", "input input input", ". . submit",
      ]),
    ]),
  ])
  |> h.form(attributes, children)
}

pub fn search_title_() {
  css.class([
    css.font_family("Lexend"),
    css.display("flex"),
    css.align_items("center"),
    css.gap(px(12)),
    css.color("var(--text-color)"),
  ])
}

pub fn search_title(attributes, children) {
  css.class([css.compose(search_title_()), css.font_size(length.rem(2.5))])
  |> h.div(attributes, children)
}

pub fn search_title_wrapper(attributes, children) {
  css.class([
    css.grid_area("title"),
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(9)),
    css.font_size(length.rem(0.9)),
    css.color(palette.dark.dark_white),
    css.line_height("1.3"),
  ])
  |> h.div(attributes, children)
}

pub fn search_lucy(size, attributes) {
  css.class([css.width(px(size))])
  |> h.img(attributes)
}

pub fn search_submit(attributes) {
  css.class([
    css.grid_area("submit"),
    css.appearance("none"),
    css.border("none"),
    css.background(palette.dark.faff_pink),
    css.border_radius(px(50)),
    css.padding_top(px(12)),
    css.padding_bottom(px(12)),
    css.padding_right(px(24)),
    css.padding_left(px(24)),
    css.color(palette.dark.charcoal),
    css.font_size(length.rem(1.0)),
    css.outline("none"),
    css.transition("background .3s"),
    css.active([css.background(palette.dark.dark_faff_pink)]),
    css.focus([css.background(palette.dark.dark_faff_pink)]),
    css.disabled([css.background(palette.dark.unexpected_aubergine)]),
  ])
  |> h.input(attributes)
}

pub fn matches_titles(attributes, children) {
  css.class([
    css.line_height("1.3"),
    css.color(palette.dark.dark_white),
    css.display("flex"),
    css.align_items("baseline"),
    css.gap(px(6)),
    css.font_size(px(12)),
    css.media(media.max_width(px(700)), [css.flex_direction("column")]),
  ])
  |> h.div(attributes, children)
}

pub fn matches_title(attributes, children) {
  css.class([css.color(palette.dark.white), css.font_size(px(18))])
  |> h.div(attributes, children)
}

pub fn empty_state(attributes, children) {
  css.class([
    css.grid_row("span 3"),
    css.display("flex"),
    css.align_items("center"),
    css.gap(px(24)),
    css.justify_content("center"),
    css.max_width(px(700)),
    css.width(length.percent(100)),
    css.margin_("auto"),
    css.padding(px(24)),
  ])
  |> h.div(attributes, children)
}

pub fn empty_state_lucy(attributes) {
  css.class([css.width(px(100))])
  |> h.img(attributes)
}

pub fn empty_state_titles(attributes, children) {
  css.class([
    css.font_size(px(20)),
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(9)),
    css.line_height("1.3"),
    css.max_width(px(400)),
  ])
  |> h.div(attributes, children)
}

pub fn empty_state_subtitle(attributes, children) {
  css.class([css.font_size(px(16)), css.color(palette.dark.dark_white)])
  |> h.div(attributes, children)
}

pub fn sidebar_wrapper(attributes, children) {
  css.class([
    css.position("sticky"),
    css.top(px(0)),
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(12)),
    css.height(vh(100)),
    css.overflow("auto"),
    css.padding_("12px"),
    css.border_right("1px solid rgba(254, 254, 252, .1)"),
    css.property("mask", {
      "linear-gradient(
        180deg,
        rgba(255,255,255, 0) 0%,
        rgba(255,255,255,1) 0.5% 99.5%,
        rgba(255, 255, 255, 0)
      )"
    }),
    css.media(media.max_width(px(700)), [css.display("none")]),
  ])
  |> h.div(attributes, children)
}

pub fn sidebar_wrapper_title(attrs, children) {
  css.class([css.padding_("4px 4px"), css.color("#ffffff99")])
  |> h.div(attrs, children)
}

pub fn sidebar_package_name(attributes, children) {
  css.class([
    css.padding_left(px(8)),
    css.border_radius(px(6)),
    css.overflow("hidden"),
    css.text_overflow("ellipsis"),
  ])
  |> h.div(attributes, children)
}

pub fn sidebar_module_name(attributes, children) {
  css.class([
    css.text_overflow("ellipsis"),
    css.overflow("hidden"),
    css.text_align("left"),
    css.cursor("pointer"),
    css.hover([css.text_decoration("underline")]),
    css.appearance("none"),
    css.background("inherit"),
    css.border("none"),
    css.color("inherit"),
    css.font_size_("inherit"),
    css.line_height("inherit"),
    css.display("block"),
    css.property("padding-block", "0"),
    css.property("padding-inline", "0"),
    css.padding_left(px(16)),
  ])
  |> h.button(attributes, children)
}

pub fn sidebar_package_wrapper(attributes, children) {
  css.class([
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(9)),
    css.font_size(px(14)),
    css.max_width(px(250)),
  ])
  |> h.div(attributes, children)
}

pub fn main(attributes, children) {
  css.class([
    css.grid_area("main"),
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(24)),
  ])
  |> h.main(attributes, children)
}

pub fn sidebar_title(attrs, children) {
  css.class([
    css.display("flex"),
    css.align_items("center"),
    css.gap(px(16)),
    css.color("inherit"),
    css.text_decoration("none"),
  ])
  |> h.a(attrs, children)
}

pub fn sidebar_title_inside(attrs, children) {
  css.class([]) |> h.div(attrs, children)
}

pub fn form_wrapper(attrs, children) {
  css.class([]) |> h.form(attrs, children)
}

pub fn sidebar_filter(attrs, children) {
  css.class([
    css.padding_top(px(12)),
    css.padding_left(px(12)),
    css.color("rgba(254, 254, 252, .6)"),
  ])
  |> h.div(attrs, children)
}

pub fn sidebar_filters(attrs, children) {
  css.class([
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(18)),
    css.padding_("0 12px"),
  ])
  |> h.div(attrs, children)
}

pub fn sidebar_checkbox(active, attrs) {
  element.fragment([
    css.class([
      css.width(px(16)),
      css.height(px(16)),
      css.background(case active {
        True -> "#ffaff3"
        False -> "rgba(254, 254, 252, .1)"
      }),
      css.border("1px solid rgba(254, 254, 252, .1)"),
      css.border_radius(px(4)),
    ])
      |> h.div([], []),
    css.class([
      css.position("fixed"),
      css.top(px(-1000)),
      css.width(px(1)),
      css.height(px(1)),
    ])
      |> h.input([a.type_("checkbox"), a.checked(active), ..attrs]),
  ])
}

pub fn sidebar_spacer(attrs, children) {
  css.class([css.flex("1")]) |> h.div(attrs, children)
}

pub fn filter_separator(attrs, children) {
  css.class([css.height(px(1)), css.background("rgba(254, 254, 252, .1)")])
  |> h.div(attrs, children)
}

pub fn sidebar_filter_line(attrs, children) {
  css.class([
    css.display("flex"),
    css.gap(px(9)),
    css.cursor("pointer"),
    css.align_items("center"),
  ])
  |> h.label(attrs, children)
}

pub fn sidebar_filter_name(attrs, children) {
  css.class([css.color("white")])
  |> h.div(attrs, children)
}

pub fn sidebar_links(attrs, children) {
  css.class([
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(24)),
    css.padding(px(12)),
  ])
  |> h.div(attrs, children)
}

pub fn sidebar_link_wrapper(attrs, children) {
  css.class([
    css.display("flex"),
    css.align_items("baseline"),
    css.gap(px(9)),
    css.text_decoration("none"),
    css.color("inherit"),
  ])
  |> h.a(attrs, children)
}

pub fn sidebar_icon(attrs, children) {
  css.class([css.width(px(12)), css.height(px(12))])
  |> h.div(attrs, children)
}

pub fn sidebar_link(attrs, children) {
  css.class([css.font_size(px(14)), css.color("rgba(254, 254, 252, 0.6)")])
  |> h.div(attrs, children)
}

pub fn items_wrapper(attributes, children) {
  css.class([
    css.display("flex"),
    css.flex_direction("column"),
    css.gap(px(24)),
    css.padding_top(px(24)),
    css.max_width(px(700)),
    css.width(length.percent(100)),
    css.overflow("hidden"),
  ])
  |> h.div(attributes, children)
}

pub fn named_type_button(attributes, children) {
  css.class([css.color("#e5c07b")])
  |> h.a(attributes, children)
}

pub fn search_title_with_hint(attributes, children) {
  css.class([css.display("flex"), css.gap(px(12))])
  |> h.div(attributes, children)
}

pub fn pre_alpha_title(attributes, children) {
  css.class([css.font_size(px(16))])
  |> h.div(attributes, children)
}

pub fn loading_trending(attributes, children) {
  css.class([])
  |> h.div(attributes, children)
}

pub fn no_trendings(attributes, children) {
  css.class([]) |> h.div(attributes, children)
}

pub fn trendings_wrapper(attributes, children) {
  css.class([css.padding_("12px 48px")]) |> h.div(attributes, children)
}

pub fn trendings_title(attributes, children) {
  css.class([css.margin_bottom(px(24))]) |> h.div(attributes, children)
}

pub fn trendings_grid(attributes, children) {
  css.class([
    css.display("grid"),
    // css.grid_template_columns("repeat(auto-fill, minmax(350px, 1fr))"),
    css.align_items("start"),
    css.gap(px(24)),
  ])
  |> h.div(attributes, children)
}

pub fn trendings_card(attributes, children) {
  css.class([css.background(palette.dark.unexpected_aubergine)])
  |> h.div(attributes, children)
}

pub fn documentation_links(attributes, children) {
  css.class([css.display("flex"), css.justify_content("space-between")])
  |> h.div(attributes, children)
}

pub fn licenses(attributes, children) {
  css.class([css.display("flex"), css.gap(px(6))])
  |> h.div(attributes, children)
}
