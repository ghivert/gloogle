import frontend/colors/palette
import lustre/attribute as a
import lustre/element
import sketch as s
import sketch/magic/element/html as h
import sketch/media
import sketch/size.{px, vh, vw}

pub fn implementations_pill_container(attrs, children) {
  s.class([
    s.display("flex"),
    s.align_items("center"),
    s.font_size(px(14)),
    s.font_weight("300"),
    s.gap(px(6)),
  ])
  |> h.div(attrs, children)
}

pub fn implementations_pill(background, attributes, children) {
  s.class([
    s.background(background),
    s.border_radius(px(6)),
    s.width(px(8)),
    s.height(px(8)),
  ])
  |> h.div(attributes, children)
}

pub fn implementations_pill_wrapper(attributes, children) {
  s.class([
    s.display("flex"),
    s.align_items("center"),
    s.gap(px(12)),
    s.justify_content("end"),
  ])
  |> h.div(attributes, children)
}

pub fn search_result(attributes, children) {
  s.class([
    s.overflow("hidden"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(12)),
  ])
  |> h.div(attributes, children)
}

pub fn search_results_wrapper(attributes, children) {
  s.class([
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
  |> h.div(attributes, children)
}

pub fn external_icon_wrapper(attrs, children) {
  s.class([s.width(px(16)), s.height(px(16))])
  |> h.div(attrs, children)
}

pub fn search_details(attributes, children) {
  s.class([
    s.display("flex"),
    s.gap(px(12)),
    s.align_items("center"),
    s.media(media.max_width(px(700)), [s.flex_direction("column")]),
  ])
  |> h.div(attributes, children)
}

pub fn search_details_title(attributes, children) {
  s.class([s.display("flex"), s.align_items("center"), s.gap(px(12))])
  |> h.div(attributes, children)
}

pub fn qualified_name(attributes, children) {
  s.class([
    s.overflow("hidden"),
    s.text_overflow("ellipsis"),
    s.text_decoration("none"),
    s.hover([s.text_decoration("underline")]),
  ])
  |> h.a(attributes, children)
}

pub fn search_body(attributes, children) {
  s.class([
    s.background("rgba(254, 254, 252, 0.05)"),
    s.border_radius(px(12)),
    s.padding_("12px 24px"),
    s.border("1px solid rgba(254, 254, 252, .1)"),
  ])
  |> h.div(attributes, children)
}

pub fn signature(attributes, children) {
  s.class([
    s.white_space("pre-wrap"),
    s.display("block"),
    s.line_height("1.6"),
    s.overflow("auto"),
  ])
  |> h.code(attributes, children)
}

pub fn documentation(attributes, children) {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(12)),
    s.padding_top(px(6)),
  ])
  |> h.div(attributes, children)
}

pub fn search_result_separator() {
  s.class([
    s.height(px(1)),
    s.background("rgba(254, 254, 252, 0.1)"),
    s.margin_("6px 0"),
  ])
  |> h.div([], [])
}

pub fn documentation_title(attributes, children) {
  s.class([s.color(palette.dark.dark_white)])
  |> h.div(attributes, children)
}

pub fn search_wrapper(attributes, children) {
  s.class([
    s.display("grid"),
    s.grid_template_rows("auto auto auto"),
    s.grid_template_columns("auto auto auto"),
    s.grid_template_areas(["title . .", "input input input", ". . submit"]),
    s.padding(px(48)),
    s.gap(px(24)),
    s.max_width(px(700)),
    s.width(size.percent(100)),
    s.margin_("auto"),
    s.media(media.max_width(px(700)), [
      s.max_width(vw(100)),
      s.padding(px(24)),
      s.grid_template_areas([
        "title title title", "input input input", ". . submit",
      ]),
    ]),
  ])
  |> h.form(attributes, children)
}

pub fn search_title_() {
  s.class([
    s.font_family("Lexend"),
    s.display("flex"),
    s.align_items("center"),
    s.gap(px(12)),
    s.color("var(--text-color)"),
  ])
}

pub fn search_title(attributes, children) {
  s.class([s.compose(search_title_()), s.font_size(size.rem(2.5))])
  |> h.div(attributes, children)
}

pub fn search_title_wrapper(attributes, children) {
  s.class([
    s.grid_area("title"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(9)),
    s.font_size(size.rem(0.9)),
    s.color(palette.dark.dark_white),
    s.line_height("1.3"),
  ])
  |> h.div(attributes, children)
}

pub fn search_lucy(size, attributes) {
  s.class([s.width(px(size))])
  |> h.img(attributes)
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
    s.font_size(size.rem(1.0)),
    s.outline("none"),
    s.transition("background .3s"),
    s.active([s.background(palette.dark.dark_faff_pink)]),
    s.focus([s.background(palette.dark.dark_faff_pink)]),
    s.disabled([s.background(palette.dark.unexpected_aubergine)]),
  ])
  |> h.input(attributes)
}

pub fn matches_titles(attributes, children) {
  s.class([
    s.line_height("1.3"),
    s.color(palette.dark.dark_white),
    s.display("flex"),
    s.align_items("baseline"),
    s.gap(px(6)),
    s.font_size(px(12)),
    s.media(media.max_width(px(700)), [s.flex_direction("column")]),
  ])
  |> h.div(attributes, children)
}

pub fn matches_title(attributes, children) {
  s.class([s.color(palette.dark.white), s.font_size(px(18))])
  |> h.div(attributes, children)
}

pub fn empty_state(attributes, children) {
  s.class([
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
  |> h.div(attributes, children)
}

pub fn empty_state_lucy(attributes) {
  s.class([s.width(px(100))])
  |> h.img(attributes)
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
  |> h.div(attributes, children)
}

pub fn empty_state_subtitle(attributes, children) {
  s.class([s.font_size(px(16)), s.color(palette.dark.dark_white)])
  |> h.div(attributes, children)
}

pub fn sidebar_wrapper(attributes, children) {
  s.class([
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
  |> h.div(attributes, children)
}

pub fn sidebar_wrapper_title(attrs, children) {
  s.class([s.padding_("4px 4px"), s.color("#ffffff99")])
  |> h.div(attrs, children)
}

pub fn sidebar_package_name(attributes, children) {
  s.class([
    s.padding_left(px(8)),
    s.border_radius(px(6)),
    s.overflow("hidden"),
    s.text_overflow("ellipsis"),
  ])
  |> h.div(attributes, children)
}

pub fn sidebar_module_name(attributes, children) {
  s.class([
    s.text_overflow("ellipsis"),
    s.overflow("hidden"),
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
  |> h.button(attributes, children)
}

pub fn sidebar_package_wrapper(attributes, children) {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(9)),
    s.font_size(px(14)),
    s.max_width(px(250)),
  ])
  |> h.div(attributes, children)
}

pub fn main(attributes, children) {
  s.class([
    s.grid_area("main"),
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(24)),
  ])
  |> h.main(attributes, children)
}

pub fn sidebar_title(attrs, children) {
  s.class([
    s.display("flex"),
    s.align_items("center"),
    s.gap(px(16)),
    s.color("inherit"),
    s.text_decoration("none"),
  ])
  |> h.a(attrs, children)
}

pub fn sidebar_title_inside(attrs, children) {
  s.class([]) |> h.div(attrs, children)
}

pub fn form_wrapper(attrs, children) {
  s.class([]) |> h.form(attrs, children)
}

pub fn sidebar_filter(attrs, children) {
  s.class([
    s.padding_top(px(12)),
    s.padding_left(px(12)),
    s.color("rgba(254, 254, 252, .6)"),
  ])
  |> h.div(attrs, children)
}

pub fn sidebar_filters(attrs, children) {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(18)),
    s.padding_("0 12px"),
  ])
  |> h.div(attrs, children)
}

pub fn sidebar_checkbox(active, attrs) {
  element.fragment([
    s.class([
      s.width(px(16)),
      s.height(px(16)),
      s.background(case active {
        True -> "#ffaff3"
        False -> "rgba(254, 254, 252, .1)"
      }),
      s.border("1px solid rgba(254, 254, 252, .1)"),
      s.border_radius(px(4)),
    ])
      |> h.div([], []),
    s.class([
      s.position("fixed"),
      s.top(px(-1000)),
      s.width(px(1)),
      s.height(px(1)),
    ])
      |> h.input([a.type_("checkbox"), a.checked(active), ..attrs]),
  ])
}

pub fn sidebar_spacer(attrs, children) {
  s.class([s.flex("1")]) |> h.div(attrs, children)
}

pub fn filter_separator(attrs, children) {
  s.class([s.height(px(1)), s.background("rgba(254, 254, 252, .1)")])
  |> h.div(attrs, children)
}

pub fn sidebar_filter_line(attrs, children) {
  s.class([
    s.display("flex"),
    s.gap(px(9)),
    s.cursor("pointer"),
    s.align_items("center"),
  ])
  |> h.label(attrs, children)
}

pub fn sidebar_filter_name(attrs, children) {
  s.class([s.color("white")])
  |> h.div(attrs, children)
}

pub fn sidebar_links(attrs, children) {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(24)),
    s.padding(px(12)),
  ])
  |> h.div(attrs, children)
}

pub fn sidebar_link_wrapper(attrs, children) {
  s.class([
    s.display("flex"),
    s.align_items("baseline"),
    s.gap(px(9)),
    s.text_decoration("none"),
    s.color("inherit"),
  ])
  |> h.a(attrs, children)
}

pub fn sidebar_icon(attrs, children) {
  s.class([s.width(px(12)), s.height(px(12))])
  |> h.div(attrs, children)
}

pub fn sidebar_link(attrs, children) {
  s.class([s.font_size(px(14)), s.color("rgba(254, 254, 252, 0.6)")])
  |> h.div(attrs, children)
}

pub fn items_wrapper(attributes, children) {
  s.class([
    s.display("flex"),
    s.flex_direction("column"),
    s.gap(px(24)),
    s.padding_top(px(24)),
    s.max_width(px(700)),
    s.width(size.percent(100)),
    s.overflow("hidden"),
  ])
  |> h.div(attributes, children)
}

pub fn named_type_button(attributes, children) {
  s.class([s.color("#e5c07b")])
  |> h.a(attributes, children)
}

pub fn search_title_with_hint(attributes, children) {
  s.class([s.display("flex"), s.gap(px(12))])
  |> h.div(attributes, children)
}

pub fn pre_alpha_title(attributes, children) {
  s.class([s.font_size(px(16))])
  |> h.div(attributes, children)
}

pub fn loading_trending(attributes, children) {
  s.class([])
  |> h.div(attributes, children)
}

pub fn no_trendings(attributes, children) {
  s.class([]) |> h.div(attributes, children)
}

pub fn trendings_wrapper(attributes, children) {
  s.class([s.padding_("12px 48px")]) |> h.div(attributes, children)
}

pub fn trendings_title(attributes, children) {
  s.class([s.margin_bottom(px(24))]) |> h.div(attributes, children)
}

pub fn trendings_grid(attributes, children) {
  s.class([
    s.display("grid"),
    // s.grid_template_columns("repeat(auto-fill, minmax(350px, 1fr))"),
    s.align_items("start"),
    s.gap(px(24)),
  ])
  |> h.div(attributes, children)
}

pub fn trendings_card(attributes, children) {
  s.class([s.background(palette.dark.unexpected_aubergine)])
  |> h.div(attributes, children)
}

pub fn documentation_links(attributes, children) {
  s.class([s.display("flex"), s.justify_content("space-between")])
  |> h.div(attributes, children)
}

pub fn licenses(attributes, children) {
  s.class([s.display("flex"), s.gap(px(6))])
  |> h.div(attributes, children)
}
