import birl
import data/model.{type Model}
import data/msg.{type Msg}
import data/package
import data/search_result
import frontend/config
import frontend/errors
import frontend/ffi
import frontend/router
import frontend/view
import frontend/view/body/search_result as sr
import gleam/bool
import gleam/dict
import gleam/dynamic
import gleam/io
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import gleam/uri.{type Uri}

// import grille_pain
// import grille_pain/lustre/toast
// import grille_pain/options
import lustre
import lustre/effect
import lustre/lazy
import lustre/update
import lustre_http as http
import modem
import plinth/browser/event.{type Event}
import sketch/lustre as sketch
import sketch/options as sketch_options
import toast/error as toast_error

fn focus(on id: String, event event: Event) {
  use _ <- effect.from()
  use <- bool.guard(when: ffi.is_active(id), return: Nil)
  event.prevent_default(event)
  ffi.focus(on: id, event: event)
}

fn unfocus() {
  use _ <- effect.from()
  ffi.unfocus()
}

fn subscribe_focus() {
  use dispatch <- effect.from()
  use event <- ffi.subscribe_focus()
  case event.key(event) {
    "Escape" -> dispatch(msg.OnEscape)
    _ -> dispatch(msg.OnSearchFocus(event))
  }
}

fn subscribe_is_mobile() {
  use dispatch <- effect.from()
  use is_mobile <- ffi.suscribe_is_mobile()
  dispatch(msg.UpdateIsMobile(is_mobile))
}

pub fn main() {
  let assert Ok(cache) = sketch.setup(sketch_options.node())
  let assert Ok(_) = lazy.setup()
  let assert Ok(_) = sr.setup()
  // let assert Ok(_) =
  //   options.default()
  //   |> options.timeout(5000)
  //   |> grille_pain.setup()

  let assert Ok(_) =
    view.view
    |> sketch.compose(cache)
    |> lustre.application(init, update, _)
    |> lustre.start("#app", Nil)
}

fn decode_package(dyn) {
  dynamic.decode4(
    msg.Package,
    dynamic.field("name", dynamic.string),
    dynamic.field("repository", dynamic.string),
    dynamic.field("rank", dynamic.int),
    dynamic.field("popularity", dynamic.optional(dynamic.int)),
  )(dyn)
}

fn init(_) {
  let initial =
    modem.initial_uri()
    |> result.map(router.parse_uri)
    |> result.unwrap(router.Home)
    |> handle_route_change(model.init(), _)
  submit_search(initial.0)
  |> update.add_effect(modem.init(on_url_change))
  |> update.add_effect(router.update_page_title({ initial.0 }.route))
  |> update.add_effect(subscribe_focus())
  |> update.add_effect(subscribe_is_mobile())
  |> update.add_effect(
    http.expect_json(dynamic.list(package.decoder), msg.Trendings)
    |> http.get(config.api_endpoint() <> "/trendings", _),
  )
  |> update.add_effect(
    http.expect_json(dynamic.list(package.decoder), msg.Packages)
    |> http.get(config.api_endpoint() <> "/packages", _),
  )
  |> update.add_effect(
    msg.OnAnalytics
    |> http.expect_json(
      dynamic.decode6(
        msg.Analytics,
        dynamic.field("total", dynamic.int),
        dynamic.field("signatures", dynamic.int),
        dynamic.field("packages", dynamic.int),
        dynamic.field("timeseries", {
          dynamic.list(dynamic.decode2(
            pair.new,
            dynamic.field("count", dynamic.int),
            dynamic.field("date", fn(dyn) {
              dynamic.string(dyn)
              |> result.then(fn(t) {
                birl.parse(t)
                |> result.replace_error([])
              })
            }),
          ))
        }),
        dynamic.field("ranked", dynamic.list(decode_package)),
        dynamic.field("popular", dynamic.list(decode_package)),
      ),
      _,
    )
    |> http.get(config.api_endpoint() <> "/analytics", _),
  )
}

fn on_url_change(uri: Uri) -> Msg {
  router.parse_uri(uri)
  |> msg.OnRouteChange()
}

fn update(model: Model, msg: Msg) {
  case io.debug(msg) {
    msg.UpdateInput(content) -> update_input(model, content)
    msg.SubmitSearch -> submit_search(model)
    msg.Reset -> reset(model)
    msg.None -> update.none(model)
    msg.Packages(Ok(packages)) -> model.Model(..model, packages:) |> update.none
    msg.Packages(_) -> update.none(model)
    msg.ScrollTo(id) -> scroll_to(model, id)
    msg.OnRouteChange(route) -> handle_route_change(model, route)
    msg.Trendings(trendings) -> handle_trendings(model, trendings)
    msg.OnSearchFocus(event) ->
      update.effect(model, focus(on: "search-input", event: event))
    msg.OnEscape -> update.effect(model, unfocus())
    msg.UpdateIsMobile(is_mobile) ->
      model
      |> model.update_is_mobile(is_mobile)
      |> update.none
    msg.SearchResults(input, search_results) ->
      handle_search_results(model, input, search_results)
    msg.OnCheckFilter(filter, value) ->
      handle_oncheck_filter(model, filter, value)
    msg.OnAnalytics(analytics) -> {
      case analytics {
        Error(_) -> #(model, effect.none())
        Ok(analytics) ->
          model
          |> model.update_analytics(analytics)
          |> update.none()
      }
    }
  }
}

fn handle_oncheck_filter(model, filter, value) {
  case filter, value {
    msg.Functions, value -> model.Model(..model, keep_functions: value)
    msg.Types, value -> model.Model(..model, keep_types: value)
    msg.Aliases, value -> model.Model(..model, keep_aliases: value)
    msg.Documented, value -> model.Model(..model, keep_documented: value)
    msg.ShowOldPackages, value -> model.Model(..model, show_old_packages: value)
    msg.VectorSearch, value -> model.Model(..model, show_vector_search: value)
    msg.DocumentationSearch, value ->
      model.Model(..model, show_documentation_search: value)
  }
  |> model.update_search_results_filter
  |> update.none
}

fn update_input(model: Model, content: String) {
  model
  |> model.update_input(content)
  |> update.none
}

fn reset(model: Model) {
  model
  |> model.reset
  |> update.none
}

fn submit_search(model: Model) {
  use <- bool.guard(when: model.input == "", return: #(model, effect.none()))
  use <- bool.guard(when: model.loading, return: #(model, effect.none()))
  let new_model = model.update_submitted_input(model)
  case dict.get(new_model.search_results, new_model.submitted_input) {
    Ok(_) -> {
      let new_route = router.Search(new_model.submitted_input)
      let is_same_route = new_model.route == new_route
      use <- bool.guard(when: is_same_route, return: update.none(new_model))
      new_model
      |> update.effect({
        Some("q=" <> new_model.submitted_input)
        |> modem.push("search", _, None)
      })
      |> update.add_effect(unfocus())
    }
    Error(_) ->
      msg.SearchResults(input: model.input, result: _)
      |> http.expect_json(search_result.decode_search_results, _)
      |> http.get(config.api_endpoint() <> "/search?q=" <> model.input, _)
      |> update.effect(model.toggle_loading(new_model), _)
      |> update.add_effect(unfocus())
  }
}

fn scroll_to(model: Model, id: String) {
  ffi.scroll_to(element: id)
  |> effect.from
  |> update.effect(model, _)
}

fn handle_search_results(
  model: Model,
  input: String,
  search_results: Result(search_result.SearchResults, http.HttpError),
) {
  let toast = display_toast(search_results)
  search_results
  |> result.map(model.update_search_results(model, input, _))
  |> result.unwrap(model)
  |> model.toggle_loading
  |> update.effect(toast)
  |> update.add_effect(modem.push("/search", Some("q=" <> input), None))
}

fn handle_route_change(model: Model, route: router.Route) {
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
  |> update.effect(router.update_page_title(route))
}

fn display_toast(
  search_results: Result(search_result.SearchResults, http.HttpError),
) {
  search_results
  |> result.map_error(fn(error) {
    toast_error.describe_http_error(error)
    |> option.map(errors.capture_message)
    // |> option.map(toast.error)
    |> option.map(fn(_) { effect.none() })
  })
  |> result.unwrap_error(option.None)
  |> option.unwrap(effect.none())
}

fn handle_trendings(
  model: Model,
  trendings: Result(List(package.Package), http.HttpError),
) {
  trendings
  |> result.map(model.update_trendings(model, _))
  |> result.unwrap(model)
  |> update.none
}
