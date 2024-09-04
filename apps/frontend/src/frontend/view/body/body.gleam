import birl
import data/model.{type Model}
import data/msg
import data/search_result
import frontend/icons
import frontend/images
import frontend/router
import frontend/strings as frontend_strings
import frontend/view/search_input/search_input
import gleam/bool
import gleam/dict
import gleam/float
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import line_chart
import lustre/attribute as a
import lustre/element as el
import lustre/element/html as h
import lustre/event as e
import lustre/lazy

fn view_search_input(model: Model) {
  let has_content = {
    model.input
    |> string.length()
    |> fn(input) { input != 0 }
  }
  h.form([a.class("search-wrapper"), e.on_submit(msg.SubmitSearch)], [
    h.div([a.class("search-title-wrapper")], [
      h.div([a.class("search-title")], [
        h.img([
          a.class("search-lucy"),
          a.style([#("width", "40px")]),
          a.src("/images/lucy.svg"),
        ]),
        h.div([a.class("search-title-with-hint")], [
          h.text("Gloogle"),
          h.div([a.class("pre-alpha-title")], [h.text("RC")]),
        ]),
      ]),
      h.text(frontend_strings.gloogle_description),
    ]),
    search_input.view(model.loading, model.input, small: False),
    h.input([
      a.class("search-submit"),
      a.type_("submit"),
      a.value("Submit"),
      a.disabled(model.loading || !has_content),
    ]),
  ])
}

fn empty_state(
  image image: String,
  title title: String,
  content content: String,
) {
  h.div([a.class("empty-state")], [
    h.img([a.class("empty-state-lucy"), a.src(image)]),
    h.div([a.class("empty-state-titles")], [
      h.div([], [h.text(title)]),
      h.div([a.class("empty-state-subtitle")], [h.text(content)]),
    ]),
  ])
}

pub fn view_trending(model: Model) {
  el.none()
  // case model.trendings {
  //   None ->
  //     empty_state(
  //       image: images.loading,
  //       title: "Loading…",
  //       content: frontend_strings.loading,
  //     )
  //   Some([]) ->
  //     empty_state(
  //       image: images.internal_error,
  //       title: "Internal server error",
  //       content: frontend_strings.internal_server_error,
  //     )
  //   Some(trendings) ->
  //     s.trendings_wrapper([], [
  //       s.trendings_title([], [h.text("Trending")]),
  //       s.trendings_grid([], {
  //         use item <- list.map(trendings)
  //         s.search_result([], [
  //           s.search_details([], [
  //             s.search_details_title([], [h.text(item.name)]),
  //             s.qualified_name([], [
  //               item.repository
  //                 |> option.map(fn(r) {
  //                   h.a([a.href(r), a.target("_blank"), a.rel("noreferrer")], [
  //                     h.text("Code"),
  //                   ])
  //                 })
  //                 |> option.unwrap(el.none()),
  //               item.documentation
  //                 |> option.map(fn(d) {
  //                   h.a([a.href(d), a.target("_blank"), a.rel("noreferrer")], [
  //                     h.text("Docs"),
  //                   ])
  //                 })
  //                 |> option.unwrap(el.none()),
  //               item.hex_url
  //                 |> option.map(fn(d) {
  //                   h.a([a.href(d), a.target("_blank"), a.rel("noreferrer")], [
  //                     h.text("Hex"),
  //                   ])
  //                 })
  //                 |> option.unwrap(el.none()),
  //             ]),
  //           ]),
  //           s.search_body([], [
  //             item.description
  //               |> option.map(fn(d) { h.div([], [h.text(d)]) })
  //               |> option.unwrap(el.none()),
  //             s.documentation([], [
  //               s.documentation_links([], [
  //                 item.licenses
  //                   |> list.map(fn(d) { h.div([], [h.text(d)]) })
  //                   |> list.prepend(h.text("Licences "))
  //                   |> s.licenses([], _),
  //                 h.div([], [
  //                   h.text("Stars "),
  //                   h.text(int.to_string(item.popularity)),
  //                 ]),
  //               ]),
  //             ]),
  //           ]),
  //         ])
  //       }),
  //     ])
  // }
}

fn sidebar(model: Model) {
  use <- bool.guard(when: model.is_mobile, return: el.none())
  h.main([a.class("search-sidebar")], [
    h.a([a.class("sidebar-title"), a.href("/")], [
      h.img([
        a.class("search-lucy"),
        a.style([#("width", "32px")]),
        a.src("/images/lucy.svg"),
      ]),
      h.form([a.class("sidebar-title-inside")], [h.text("Gloogle")]),
    ]),
    h.form([e.on_submit(msg.SubmitSearch)], [
      search_input.view(model.loading, model.input, small: True),
    ]),
    h.div([a.class("sidebar-filter")], [el.text("Filters")]),
    h.div([a.class("sidebar-filters")], [
      checkbox(model.keep_functions, msg.Functions, "Functions"),
      checkbox(model.keep_types, msg.Types, "Types"),
      checkbox(model.keep_aliases, msg.Aliases, "Aliases"),
      h.div([a.class("filter-separator")], []),
      checkbox(model.keep_documented, msg.Documented, "Documented"),
      checkbox(
        model.show_old_packages,
        msg.ShowOldPackages,
        "Show old versions",
      ),
      h.div([a.class("filter-separator")], []),
      checkbox(
        model.show_documentation_search,
        msg.DocumentationSearch,
        "Documentation Search",
      ),
      checkbox(model.show_vector_search, msg.VectorSearch, "Vector Search"),
    ]),
    h.div([a.class("sidebar-spacer")], []),
    h.div([a.class("sidebar-links")], [
      sidebar_link(href: "/analytics", icon: icons.trends(), title: "Analytics"),
      // s.sidebar_link_wrapper([], [
      //   s.sidebar_icon([], [icons.shortcuts()]),
      //   s.sidebar_link([], [el.text("Shortcuts")]),
      // ]),
      // s.sidebar_link_wrapper([], [
      //   s.sidebar_icon([], [icons.gift()]),
      //   s.sidebar_link([], [el.text("Resources")]),
      // ]),
      sidebar_external_link(
        href: "https://github.com/sponsors/ghivert",
        title: "Sponsor",
        icon: icons.heart(),
      ),
    ]),
  ])
}

fn checkbox(active: Bool, msg: msg.Filter, name: String) {
  let bg = case active {
    True -> "#ffaff3"
    False -> "var(--input-background)"
  }
  h.label([a.class("sidebar-filter-line")], [
    el.fragment([
      h.div([a.class("sidebar-checkbox-1"), a.style([#("background", bg)])], []),
      h.input([
        a.class("sidebar-checkbox-2"),
        a.type_("checkbox"),
        a.checked(active),
        e.on_check(msg.OnCheckFilter(msg, _)),
      ]),
    ]),
    h.div([a.class("sidebar-filter-name")], [el.text(name)]),
  ])
}

fn sidebar_external_link(href href: String, title title: String, icon icon) {
  let class = a.class("sidebar-link-wrapper")
  h.a([class, a.href(href), a.target("_blank"), a.rel("noreferrer noopener")], [
    h.div([a.class("sidebar-icon")], [icon]),
    h.div([a.class("sidebar-link")], [el.text(title)]),
  ])
}

fn sidebar_link(href href: String, title title: String, icon icon) {
  let class = a.class("sidebar-link-wrapper")
  h.a([class, a.href(href)], [
    h.div([a.class("sidebar-icon")], [icon]),
    h.div([a.class("sidebar-link")], [el.text(title)]),
  ])
}

fn format_huge_number(number: Int) {
  let number = int.to_float(number)
  let g = number /. 1_000_000_000.0
  let m = number /. 1_000_000.0
  let k = number /. 1000.0
  case number {
    _ if g >. 1.0 -> float.to_string(g) |> string.slice(0, 5) <> " G"
    _ if m >. 1.0 -> float.to_string(m) |> string.slice(0, 5) <> " M"
    _ if k >. 1.0 -> float.to_string(k) |> string.slice(0, 5) <> " K"
    _ -> float.round(number) |> int.to_string
  }
}

pub fn body(model: Model) {
  case model.route {
    router.Home -> h.main([a.class("main")], [view_search_input(model)])
    router.Trending -> h.main([a.class("main")], [view_trending(model)])
    router.Analytics ->
      el.fragment([
        sidebar(model),
        h.main([a.class("main")], [
          h.div(
            [a.class("items-wrapper"), a.style([#("padding-left", "24px")])],
            [
              h.div([a.class("matches-titles")], [
                h.div([a.class("matches-title")], [h.text("Global analytics")]),
              ]),
              h.div([a.class("analytics-box-wrapper")], [
                h.div([a.class("analytics-box")], [
                  h.div([a.class("analytics-title")], [
                    h.text("Number of searches"),
                  ]),
                  h.text(format_huge_number(model.total_searches)),
                ]),
                h.div([a.class("analytics-box")], [
                  h.div([a.class("analytics-title")], [
                    h.text("Number of signatures indexed"),
                  ]),
                  h.text(format_huge_number(model.total_signatures)),
                ]),
                h.div([a.class("analytics-box")], [
                  h.div([a.class("analytics-title")], [
                    h.text("Number of packages indexed"),
                  ]),
                  h.text(format_huge_number(model.total_packages)),
                ]),
              ]),
              h.div([a.class("matches-titles")], [
                h.div([a.class("matches-title")], [h.text("Last 30 days")]),
              ]),
              h.div([a.style([#("width", "auto"), #("height", "500px")])], [
                case model.timeseries {
                  [] -> el.none()
                  data -> {
                    line_chart.line_chart({
                      use line_chart.Dataset(dates, value), #(count, date) <- list.fold(
                        data,
                        line_chart.Dataset([], []),
                      )
                      line_chart.Dataset([birl.to_iso8601(date), ..dates], [
                        count,
                        ..value
                      ])
                    })
                  }
                },
              ]),
            ],
          ),
        ]),
      ])
    router.Search(_) -> {
      let key = model.search_key(model.submitted_input, model)
      el.fragment([
        sidebar(model),
        case
          dict.get(model.search_results, key)
          |> result.unwrap(search_result.Start)
        {
          search_result.Start ->
            empty_state(
              image: images.loading,
              title: "Loading…",
              content: frontend_strings.loading,
            )
          search_result.InternalServerError ->
            empty_state(
              image: images.internal_error,
              title: "Internal server error",
              content: frontend_strings.internal_server_error,
            )
          search_result.SearchResults([], [], [], [], [], []) ->
            empty_state(
              image: images.shadow_lucy,
              title: "No match found!",
              content: frontend_strings.retry_query,
            )
          search_result.SearchResults(_, _, _, _, _, _) -> {
            dict.get(model.view_cache, key)
            |> result.map(lazy.lazy(_))
            |> result.unwrap(el.none())
          }
        },
      ])
    }
  }
}
