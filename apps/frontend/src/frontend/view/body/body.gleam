import birl
import chart.{Dataset}
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
import gleam/list
import gleam/option
import gleam/result
import gleam/string
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
  let disabled = case model.route {
    router.Search(..) -> a.style([#("opacity", "1")])
    _ -> a.style([#("opacity", "0.3"), #("pointer-events", "none")])
  }
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
    h.div([a.class("sidebar-filter"), disabled], [el.text("Filters")]),
    h.div([a.class("sidebar-filters"), disabled], [
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
      h.div([a.class("filter-separator"), disabled], []),
      checkbox(
        model.show_documentation_search,
        msg.DocumentationSearch,
        "Documentation Search",
      ),
      checkbox(model.show_vector_search, msg.VectorSearch, "Vector Search"),
    ]),
    h.div([a.class("sidebar-spacer"), disabled], []),
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
    _ if g >. 1.0 -> float.to_string(g) |> string.slice(0, 6) <> " G"
    _ if m >. 1.0 -> float.to_string(m) |> string.slice(0, 6) <> " M"
    _ if k >. 10.0 -> float.to_string(k) |> string.slice(0, 6) <> " K"
    _ -> float.round(number) |> int.to_string
  }
}

fn analytics_box(title: String, count: Int) {
  h.div([a.class("analytics-box")], [
    h.div([a.class("analytics-title")], [h.text(title)]),
    h.text(format_huge_number(count)),
  ])
}

fn popularity_chart(model: Model) {
  let data =
    list.filter(model.popular, fn(p) {
      p.repository != "https://github.com/gleam-lang/gleam"
    })
  use <- bool.guard(when: list.is_empty(data), return: el.none())
  chart.bar_chart("#ff851b", {
    let acc = Dataset([], [])
    use Dataset(dates, value), package <- list.fold_right(data, acc)
    case package.popularity {
      option.None -> Dataset(dates, value)
      option.Some(popularity) -> {
        let label = package.name
        Dataset([label, ..dates], [popularity, ..value])
      }
    }
  })
}

fn ranked_chart(model: Model) {
  let data =
    list.filter(model.ranked, fn(p) {
      p.name != "gleam_stdlib" && p.name != "gleeunit"
    })
  use <- bool.guard(when: list.is_empty(data), return: el.none())
  chart.bar_chart("#ffaff3", {
    let acc = Dataset([], [])
    use Dataset(dates, value), package <- list.fold_right(data, acc)
    let label = package.name
    Dataset([label, ..dates], [package.rank, ..value])
  })
}

fn analytics_chart(model: Model) {
  let data = model.timeseries
  use <- bool.guard(when: list.is_empty(data), return: el.none())
  chart.line_chart({
    let acc = Dataset([], [])
    use Dataset(dates, value), #(count, date) <- list.fold(data, acc)
    let day = birl.get_day(date)
    let label =
      [day.year, day.month, day.date]
      |> list.map(int.to_string)
      |> string.join("/")
    Dataset([label, ..dates], [count, ..value])
  })
}

fn analytics_title(title: String, border: Bool) {
  el.fragment([
    h.div([a.class("matches-titles")], [
      h.div([a.class("matches-title")], [h.text(title)]),
    ]),
    case border {
      False -> el.none()
      True ->
        h.div(
          [
            a.style([
              #("height", "1px"),
              #("max-width", "800px"),
              #("margin-bottom", "0px"),
              #("background", "var(--border-color)"),
            ]),
          ],
          [],
        )
    },
  ])
}

fn view_analytics(model: Model) {
  el.fragment([
    sidebar(model),
    h.main([a.class("main"), a.style([#("padding", "24px 36px")])], [
      analytics_title("Gloogle analytics", True),
      analytics_title("Global analytics", False),
      h.div([a.class("analytics-box-wrapper")], [
        analytics_box("Number of searches", model.total_searches),
        analytics_box("Number of signatures indexed", model.total_signatures),
        analytics_box("Number of packages indexed", model.total_packages),
      ]),
      analytics_title("Searches per day — Last 30 days", False),
      h.div([a.class("analytics-charts-wrapper")], [analytics_chart(model)]),
      analytics_title("Gleam ecosystem analytics", True),
      analytics_title("Popular packages — Stars on GitHub", False),
      h.div([a.class("analytics-box-wrapper")], [
        analytics_box(
          "Stars on gleam-lang/glang",
          model.popular
            |> list.find(fn(p) {
              p.repository == "https://github.com/gleam-lang/gleam"
            })
            |> result.map(fn(p) { p.popularity |> option.unwrap(0) })
            |> result.unwrap(0),
        ),
      ]),
      h.div([a.class("analytics-charts-wrapper")], [popularity_chart(model)]),
      analytics_title(
        "Most used packages — Used as dependencies on Hex, counted by release",
        False,
      ),
      h.div([a.class("analytics-box-wrapper")], [
        analytics_box(
          "Packages releases using gleam_stdlib",
          model.ranked
            |> list.find(fn(p) { p.name == "gleam_stdlib" })
            |> result.map(fn(p) { p.rank })
            |> result.unwrap(0),
        ),
        analytics_box(
          "Packages releases using gleeunit",
          model.ranked
            |> list.find(fn(p) { p.name == "gleeunit" })
            |> result.map(fn(p) { p.rank })
            |> result.unwrap(0),
        ),
      ]),
      h.div([a.class("analytics-charts-wrapper")], [ranked_chart(model)]),
    ]),
  ])
}

pub fn body(model: Model) {
  case model.route {
    router.Home -> h.main([a.class("main")], [view_search_input(model)])
    router.Trending -> h.main([a.class("main")], [view_trending(model)])
    router.Analytics -> view_analytics(model)
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
