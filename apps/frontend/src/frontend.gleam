import bright
import data/analytics
import data/model.{type Model}
import data/msg.{type Msg}
import data/package
import frontend/discuss
import frontend/effects/window
import frontend/router
import frontend/update
import frontend/view
import frontend/view/body/search_result as sr
import gleam/dynamic

import gleam/result

import grille_pain

import grille_pain/options
import lustre
import lustre/effect
import lustre/lazy
import modem
import sketch
import sketch/magic

pub fn main() {
  let assert Ok(_) = setup_sketch()
  let assert Ok(_) = setup_components()
  let assert Ok(_) = setup_grille_pain()
  let assert Ok(_) = start_application()
}

fn setup_sketch() {
  use cache <- result.try(sketch.cache(strategy: sketch.Ephemeral))
  use _ <- result.try(magic.setup(cache))
  Ok(Nil)
}

fn setup_components() {
  use _ <- result.try(lazy.setup())
  use _ <- result.try(sr.setup())
  Ok(Nil)
}

fn setup_grille_pain() {
  options.default()
  |> options.timeout(5000)
  |> grille_pain.setup()
}

fn start_application() {
  lustre.application(init, update, view.view)
  |> lustre.start("#app", Nil)
}

fn get_initial_uri() {
  modem.initial_uri()
  |> result.map(router.parse_uri)
  |> result.unwrap(router.Home)
}

fn init_modem() {
  use uri <- modem.init
  router.parse_uri(uri)
  |> msg.BrowserChangedRoute
}

fn get_trendings() {
  use dispatch <- effect.from
  discuss.about(["trendings"])
  |> discuss.expect(dynamic.list(package.decode))
  |> discuss.on_success(fn(m) { dispatch(msg.ApiReturnedTrendings(m)) })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}

fn get_packages() {
  use dispatch <- effect.from
  discuss.about(["packages"])
  |> discuss.expect(dynamic.list(package.decode))
  |> discuss.on_success(fn(m) { dispatch(msg.ApiReturnedPackages(m)) })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}

fn get_analytics() {
  use dispatch <- effect.from
  discuss.about(["analytics"])
  |> discuss.expect(analytics.decode)
  |> discuss.on_success(fn(m) { dispatch(msg.ApiReturnedAnalytics(m)) })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  let uri = get_initial_uri()
  let model = bright.init(model.init_data(), model.Computed)
  use model <- bright.start(model)
  use model <- bright.update(model, update.handle_changed_route(_, uri))
  use model <- bright.update(model, update.handle_submitted_search)
  model
  |> bright.run(fn(_, _) { init_modem() })
  |> bright.run(fn(data, _) { router.update_page_title(data.route) })
  |> bright.run(fn(_, _) { window.subscribe_focus() })
  |> bright.run(fn(_, _) { window.subscribe_is_mobile() })
  |> bright.run(fn(_, _) { get_trendings() })
  |> bright.run(fn(_, _) { get_packages() })
  |> bright.run(fn(_, _) { get_analytics() })
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  use model <- bright.start(model)
  use model <- bright.update(model, update_data(_, msg))
  model
}

fn update_data(model: model.Data, msg: Msg) {
  case msg {
    msg.ApiReturnedAnalytics(analytics:) ->
      update.handle_analytics(model, analytics)
    msg.ApiReturnedPackages(packages:) ->
      update.handle_packages(model, packages)
    msg.ApiReturnedSearchResults(input:, search_results:) ->
      update.handle_search_results(model, input, search_results)
    msg.ApiReturnedTrendings(trendings:) ->
      update.handle_trendings(model, trendings)
    msg.AppRequiredDiscussToast(message:) ->
      update.handle_discuss_toast(model, message)
    msg.BrowserChangedRoute(route:) -> update.handle_changed_route(model, route)
    msg.BrowserResizedViewport(is_mobile:) ->
      update.handle_resized_viewport(model, is_mobile)
    msg.UserClickedSidebarName(id:) ->
      update.handle_clicked_sidebar_name(model, id)
    msg.UserFocusedSearch(event:) -> update.handle_focused_search(model, event)
    msg.UserInputtedSearch(query:) ->
      update.handle_inputted_search(model, query)
    msg.UserPressedEscape -> update.handle_pressed_escape(model)
    msg.UserSubmittedSearch -> update.handle_submitted_search(model)
    msg.UserToggledFilter(filter:, value:) ->
      update.handle_toggle_filter(model, filter, value)
  }
}
