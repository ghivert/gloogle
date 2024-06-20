import data/model.{type Model}
import data/msg
import data/search_result
import frontend/images
import frontend/router
import frontend/strings as frontend_strings
import frontend/view/body/styles as s
import frontend/view/search_input/search_input
import gleam/dict
import gleam/int
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
  s.search_wrapper([e.on_submit(msg.SubmitSearch)], [
    s.search_title_wrapper([], [
      s.search_title([], [
        s.search_lucy([a.src("/images/lucy.svg")]),
        s.search_title_with_hint([], [
          h.text("Gloogle"),
          s.pre_alpha_title([], [h.text("Alpha")]),
        ]),
      ]),
      h.text(frontend_strings.gloogle_description),
    ]),
    search_input.view(model.loading, model.input, True),
    s.search_submit([
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
  s.empty_state([], [
    s.empty_state_lucy([a.src(image)]),
    s.empty_state_titles([], [
      h.div([], [h.text(title)]),
      s.empty_state_subtitle([], [h.text(content)]),
    ]),
  ])
}

pub fn view_trending(model: Model) {
  case model.trendings {
    None ->
      empty_state(
        image: images.loading,
        title: "Loading…",
        content: frontend_strings.loading,
      )
    Some([]) ->
      empty_state(
        image: images.internal_error,
        title: "Internal server error",
        content: frontend_strings.internal_server_error,
      )
    Some(trendings) ->
      s.trendings_wrapper([], [
        s.trendings_title([], [h.text("Trending")]),
        s.trendings_grid([], {
          use item <- list.map(trendings)
          s.search_result([], [
            s.search_details([], [
              s.search_details_title([], [h.text(item.name)]),
              s.qualified_name([], [
                item.repository
                  |> option.map(fn(r) {
                    h.a([a.href(r), a.target("_blank"), a.rel("noreferrer")], [
                      h.text("Code"),
                    ])
                  })
                  |> option.unwrap(el.none()),
                item.documentation
                  |> option.map(fn(d) {
                    h.a([a.href(d), a.target("_blank"), a.rel("noreferrer")], [
                      h.text("Docs"),
                    ])
                  })
                  |> option.unwrap(el.none()),
                item.hex_url
                  |> option.map(fn(d) {
                    h.a([a.href(d), a.target("_blank"), a.rel("noreferrer")], [
                      h.text("Hex"),
                    ])
                  })
                  |> option.unwrap(el.none()),
              ]),
            ]),
            s.search_body([], [
              item.description
                |> option.map(fn(d) { h.div([], [h.text(d)]) })
                |> option.unwrap(el.none()),
              s.documentation([], [
                s.documentation_links([], [
                  item.licenses
                    |> list.map(fn(d) { h.div([], [h.text(d)]) })
                    |> list.prepend(h.text("Licences "))
                    |> s.licenses([], _),
                  h.div([], [
                    h.text("Stars "),
                    h.text(int.to_string(item.popularity)),
                  ]),
                ]),
              ]),
            ]),
          ])
        }),
      ])
  }
}

pub fn body(model: Model) {
  s.main([], [
    case model.route {
      router.Home -> view_search_input(model)
      router.Trending -> view_trending(model)
      router.Search(_) ->
        case
          dict.get(model.search_results, model.submitted_input)
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
          search_result.SearchResults([], [], [], [], []) ->
            empty_state(
              image: images.shadow_lucy,
              title: "No match found!",
              content: frontend_strings.retry_query,
            )
          search_result.SearchResults(_, _, _, _, _) -> {
            dict.get(model.view_cache, model.submitted_input)
            |> result.map(fn(content) {
              el.element(
                "cache-signatures",
                [a.property("content", content)],
                [],
              )
            })
            |> result.unwrap(el.none())
          }
        }
    },
  ])
}
