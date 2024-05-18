import data/model.{type Model}
import data/msg
import data/search_result
import frontend/images
import frontend/router
import frontend/strings as frontend_strings
import frontend/view/body/styles as s
import frontend/view/search_input/search_input
import lustre/attribute as a
import lustre/element/html as h
import lustre/event as e

fn view_search_input(model: Model) {
  s.search_wrapper([e.on_submit(msg.SubmitSearch)], [
    s.search_title_wrapper([], [
      s.search_title([], [
        s.search_lucy([a.src("/images/lucy.svg")]),
        s.search_title_with_hint([], [
          h.text("Gloogle"),
          s.pre_alpha_title([], [h.text("Pre Alpha")]),
        ]),
      ]),
      h.text(frontend_strings.gloogle_description),
    ]),
    search_input.view(model.loading, model.input),
    s.search_submit([
      a.type_("submit"),
      a.value("Submit"),
      a.disabled(model.loading),
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

pub fn body(model: Model) {
  s.main([], [
    case model.route {
      router.Home -> view_search_input(model)
      router.Search(_) ->
        case model.search_results {
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
          search_result.SearchResults([], [], []) ->
            empty_state(
              image: images.shadow_lucy,
              title: "No match found!",
              content: frontend_strings.retry_query,
            )
          search_result.SearchResults(_, _, _) -> model.view_cache
        }
    },
  ])
}
