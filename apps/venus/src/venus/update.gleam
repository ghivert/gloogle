import data/analytics
import data/model.{type State}
import data/msg
import data/package
import data/search_result
import gleam/bool
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option
import gleam/pair
import grille_pain/lustre/toast
import lustre/effect
import toast/error as toast_error
import venus/discuss
import venus/effects/api
import venus/effects/window
import venus/errors
import venus/router

pub fn handle_analytics(state: State, analytics: analytics.Analytics) {
  state
  |> model.update_analytics(analytics)
  |> pair.new(effect.none())
}

pub fn handle_packages(state: State, packages: List(package.Package)) {
  model.State(..state, packages:)
  |> pair.new(effect.none())
}

pub fn handle_search_results(
  state: State,
  input: String,
  search_results: search_result.SearchResults,
) {
  search_results
  |> model.update_search_results(state, input, _)
  |> model.toggle_loading
  |> pair.new(router.push(router.Search("q=" <> input)))
}

pub fn handle_trendings(state: State, trendings: List(package.Package)) {
  trendings
  |> model.update_trendings(state, _)
  |> pair.new(effect.none())
}

pub fn handle_discuss_toast(state: State, message: discuss.DiscussError) {
  message
  |> toast_error.describe_http_error
  |> option.map(errors.capture_message)
  |> option.map(toast.error)
  |> option.unwrap(effect.none())
  |> pair.new(state, _)
}

pub fn handle_changed_route(state: State, route: router.Route) {
  let model = model.update_route(state, route)
  case route {
    router.Home -> model.update_input(model, "")
    router.Packages -> model.update_input(model, "")
    router.Trending -> model.update_input(model, "")
    router.Analytics -> model.update_input(model, "")
    router.Search(q) ->
      model.update_input(model, q)
      |> model.update_submitted_input
  }
  |> pair.new(router.update_page_title(route))
}

pub fn handle_resized_viewport(state: State, is_mobile: Bool) {
  state
  |> model.update_is_mobile(is_mobile)
  |> pair.new(effect.none())
}

pub fn handle_clicked_sidebar_name(state: State, id: String) {
  window.scroll_to(element: id)
  |> pair.new(state, _)
}

pub fn handle_focused_search(state: State, event: Dynamic) {
  #(state, window.focus(on: "search-input", event: event))
}

pub fn handle_inputted_search(state: State, content: String) {
  state
  |> model.update_input(content)
  |> pair.new(effect.none())
}

pub fn handle_pressed_escape(state: State) {
  #(state, window.blur())
}

pub fn handle_submitted_search(state: State) {
  use <- bool.guard(when: state.input == "", return: #(state, effect.none()))
  use <- bool.guard(when: state.loading, return: #(state, effect.none()))
  let new_state = model.update_submitted_input(state)
  case dict.get(new_state.search_results, new_state.submitted_input) {
    Ok(_) -> {
      let new_route = router.Search(new_state.submitted_input)
      let is_same_route = new_state.route == new_route
      use <- bool.guard(when: is_same_route, return: #(new_state, effect.none()))
      [router.push(router.Search("q=" <> new_state.submitted_input))]
      |> list.prepend(window.blur())
      |> effect.batch
      |> pair.new(new_state, _)
    }
    Error(_) -> {
      let effects = effect.batch([api.get_search(state), window.blur()])
      model.toggle_loading(new_state)
      |> pair.new(effects)
    }
  }
}

pub fn handle_toggle_filter(state, filter, value) {
  case filter, value {
    msg.Functions, value -> model.State(..state, keep_functions: value)
    msg.Types, value -> model.State(..state, keep_types: value)
    msg.Aliases, value -> model.State(..state, keep_aliases: value)
    msg.Documented, value -> model.State(..state, keep_documented: value)
    msg.ShowOldPackages, value -> model.State(..state, show_old_packages: value)
    msg.VectorSearch, value -> model.State(..state, show_vector_search: value)
    msg.DocumentationSearch, value ->
      model.State(..state, show_documentation_search: value)
  }
  |> model.update_search_results_filter
  |> pair.new(effect.none())
}
