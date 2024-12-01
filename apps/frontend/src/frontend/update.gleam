import data/analytics
import data/model.{type Data}
import data/msg
import data/package
import data/search_result
import frontend/discuss
import frontend/effects/api
import frontend/effects/window
import frontend/errors
import frontend/router
import gleam/bool
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/option
import gleam/pair
import grille_pain/lustre/toast
import lustre/effect
import toast/error as toast_error

pub fn handle_analytics(model: Data, analytics: analytics.Analytics) {
  model
  |> model.update_analytics(analytics)
  |> pair.new(effect.none())
}

pub fn handle_packages(model: Data, packages: List(package.Package)) {
  model.Data(..model, packages:)
  |> pair.new(effect.none())
}

pub fn handle_search_results(
  model: Data,
  input: String,
  search_results: search_result.SearchResults,
) {
  search_results
  |> model.update_search_results(model, input, _)
  |> model.toggle_loading
  |> pair.new(router.push(router.Search("q=" <> input)))
}

pub fn handle_trendings(model: Data, trendings: List(package.Package)) {
  trendings
  |> model.update_trendings(model, _)
  |> pair.new(effect.none())
}

pub fn handle_discuss_toast(model: Data, message: discuss.DiscussError) {
  message
  |> toast_error.describe_http_error
  |> option.map(errors.capture_message)
  |> option.map(toast.error)
  |> option.unwrap(effect.none())
  |> pair.new(model, _)
}

pub fn handle_changed_route(model: Data, route: router.Route) {
  let model = model.update_route(model, route)
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

pub fn handle_resized_viewport(model: Data, is_mobile: Bool) {
  model
  |> model.update_is_mobile(is_mobile)
  |> pair.new(effect.none())
}

pub fn handle_clicked_sidebar_name(model: Data, id: String) {
  window.scroll_to(element: id)
  |> pair.new(model, _)
}

pub fn handle_focused_search(model: Data, event: Dynamic) {
  #(model, window.focus(on: "search-input", event: event))
}

pub fn handle_inputted_search(model: Data, content: String) {
  model
  |> model.update_input(content)
  |> pair.new(effect.none())
}

pub fn handle_pressed_escape(model: Data) {
  #(model, window.blur())
}

pub fn handle_submitted_search(model: Data) {
  use <- bool.guard(when: model.input == "", return: #(model, effect.none()))
  use <- bool.guard(when: model.loading, return: #(model, effect.none()))
  let new_model = model.update_submitted_input(model)
  case dict.get(new_model.search_results, new_model.submitted_input) {
    Ok(_) -> {
      let new_route = router.Search(new_model.submitted_input)
      let is_same_route = new_model.route == new_route
      use <- bool.guard(when: is_same_route, return: #(new_model, effect.none()))
      [router.push(router.Search("q=" <> new_model.submitted_input))]
      |> list.prepend(window.blur())
      |> effect.batch
      |> pair.new(new_model, _)
    }
    Error(_) -> {
      let effects = effect.batch([api.get_search(model), window.blur()])
      model.toggle_loading(new_model)
      |> pair.new(effects)
    }
  }
}

pub fn handle_toggle_filter(model, filter, value) {
  case filter, value {
    msg.Functions, value -> model.Data(..model, keep_functions: value)
    msg.Types, value -> model.Data(..model, keep_types: value)
    msg.Aliases, value -> model.Data(..model, keep_aliases: value)
    msg.Documented, value -> model.Data(..model, keep_documented: value)
    msg.ShowOldPackages, value -> model.Data(..model, show_old_packages: value)
    msg.VectorSearch, value -> model.Data(..model, show_vector_search: value)
    msg.DocumentationSearch, value ->
      model.Data(..model, show_documentation_search: value)
  }
  |> model.update_search_results_filter
  |> pair.new(effect.none())
}
