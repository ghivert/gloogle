import data/model.{type Model}
import data/msg
import data/search_result
import frontend/icons
import frontend/images
import frontend/router
import frontend/strings as frontend_strings
import frontend/view/search_input/search_input
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import lustre/attribute as a
import lustre/element as el
import lustre/element/html as h
import lustre/event as e

fn view_search_input(model: Model) {
  let has_content = {
    model.input
    |> string.split(" ")
    |> list.filter(fn(word) { !list.contains(search_input.valid_filters, word) })
    |> string.join(" ")
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
          h.div([a.class("pre-alpha-title")], [h.text("Beta")]),
        ]),
      ]),
      h.text(frontend_strings.gloogle_description),
    ]),
    search_input.view(
      model.loading,
      model.input,
      show_filters: True,
      small: False,
    ),
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

fn on_coerce(value: a) {
  Ok(coerce_event(value))
}

@external(javascript, "../../../config.ffi.mjs", "coerce_event")
fn coerce_event(value: a) -> b

fn sidebar(model: Model) {
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
      search_input.view(
        model.loading,
        model.input,
        show_filters: False,
        small: True,
      ),
    ]),
    h.div([a.class("sidebar-filter")], [el.text("Filters")]),
    h.div([a.class("sidebar-filters")], [
      h.label([a.class("sidebar-filter-line")], [
        el.fragment([
          h.div(
            [
              a.class("sidebar-checkbox-1"),
              a.style([
                #("background", case model.keep_functions {
                  True -> "#ffaff3"
                  False -> "var(--input-background)"
                }),
              ]),
            ],
            [],
          ),
          h.input([
            a.class("sidebar-checkbox-2"),
            a.type_("checkbox"),
            a.checked(model.keep_functions),
            e.on_check(msg.OnCheckFilter(msg.Functions, _)),
          ]),
        ]),
        h.div([a.class("sidebar-filter-name")], [el.text("Functions")]),
      ]),
      h.label([a.class("sidebar-filter-line")], [
        el.fragment([
          h.div(
            [
              a.class("sidebar-checkbox-1"),
              a.style([
                #("background", case model.keep_types {
                  True -> "#ffaff3"
                  False -> "var(--input-background)"
                }),
              ]),
            ],
            [],
          ),
          h.input([
            a.class("sidebar-checkbox-2"),
            a.type_("checkbox"),
            a.checked(model.keep_types),
            e.on_check(msg.OnCheckFilter(msg.Types, _)),
          ]),
        ]),
        h.div([a.class("sidebar-filter-name")], [el.text("Types")]),
      ]),
      h.label([a.class("sidebar-filter-line")], [
        el.fragment([
          h.div(
            [
              a.class("sidebar-checkbox-1"),
              a.style([
                #("background", case model.keep_aliases {
                  True -> "#ffaff3"
                  False -> "var(--input-background)"
                }),
              ]),
            ],
            [],
          ),
          h.input([
            a.class("sidebar-checkbox-2"),
            a.type_("checkbox"),
            a.checked(model.keep_aliases),
            e.on_check(msg.OnCheckFilter(msg.Aliases, _)),
          ]),
        ]),
        h.div([a.class("sidebar-filter-name")], [el.text("Aliases")]),
      ]),
      h.div([a.class("filter-separator")], []),
      h.label([a.class("sidebar-filter-line")], [
        el.fragment([
          h.div(
            [
              a.class("sidebar-checkbox-1"),
              a.style([
                #("background", case model.keep_documented {
                  True -> "#ffaff3"
                  False -> "var(--input-background)"
                }),
              ]),
            ],
            [],
          ),
          h.input([
            a.class("sidebar-checkbox-2"),
            a.type_("checkbox"),
            a.checked(model.keep_documented),
            e.on_check(msg.OnCheckFilter(msg.Documented, _)),
          ]),
        ]),
        h.div([a.class("sidebar-filter-name")], [el.text("Documented")]),
      ]),
      h.label([a.class("sidebar-filter-line")], [
        el.fragment([
          h.div(
            [
              a.class("sidebar-checkbox-1"),
              a.style([
                #("background", case model.show_old_packages {
                  True -> "#ffaff3"
                  False -> "var(--input-background)"
                }),
              ]),
            ],
            [],
          ),
          h.input([
            a.class("sidebar-checkbox-2"),
            a.type_("checkbox"),
            a.checked(model.show_old_packages),
            e.on_check(msg.OnCheckFilter(msg.ShowOldPackages, _)),
          ]),
        ]),
        h.div([a.class("sidebar-filter-name")], [el.text("Show old versions")]),
      ]),
    ]),
    h.div([a.class("sidebar-spacer")], []),
    h.div([a.class("sidebar-links")], [
      // s.sidebar_link_wrapper([], [
      //   s.sidebar_icon([], [icons.trends()]),
      //   s.sidebar_link([], [el.text("Trends")]),
      // ]),
      // s.sidebar_link_wrapper([], [
      //   s.sidebar_icon([], [icons.shortcuts()]),
      //   s.sidebar_link([], [el.text("Shortcuts")]),
      // ]),
      // s.sidebar_link_wrapper([], [
      //   s.sidebar_icon([], [icons.gift()]),
      //   s.sidebar_link([], [el.text("Resources")]),
      // ]),
      h.a(
        [
          a.class("sidebar-link-wrapper"),
          a.href("https://github.com/sponsors/ghivert"),
          a.target("_blank"),
          a.rel("noreferrer noopener"),
        ],
        [
          h.div([a.class("sidebar-icon")], [icons.heart()]),
          h.div([a.class("sidebar-link")], [el.text("Sponsor")]),
        ],
      ),
    ]),
  ])
}

pub fn body(model: Model) {
  case model.route {
    router.Home -> h.main([a.class("main")], [view_search_input(model)])
    router.Trending -> h.main([a.class("main")], [view_trending(model)])
    router.Search(_) -> {
      let key =
        model.submitted_input
        <> string.inspect([
          model.keep_functions,
          model.keep_types,
          model.keep_aliases,
          model.keep_documented,
          model.show_old_packages,
        ])
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
            |> result.map(fn(content) {
              el.element(
                "cache-signatures",
                [a.property("content", content), e.on("child", on_coerce)],
                [],
              )
            })
            |> result.unwrap(el.none())
          }
        },
      ])
    }
  }
}
