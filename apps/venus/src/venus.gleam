import bright
import data/model.{type Model, type State}
import data/msg.{type Msg}
import lustre
import lustre/effect
import sketch/lustre as sl
import venus/effects/api
import venus/effects/window
import venus/router
import venus/setup
import venus/update
import venus/view

pub fn main() {
  let assert Ok(stylesheet) = sl.setup()
  let assert Ok(_) = setup.components()
  let assert Ok(_) = setup.grille_pain()
  let assert Ok(_) =
    view.view(_, stylesheet)
    |> lustre.application(init, update, _)
    |> lustre.start("#app", Nil)
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  let route = setup.initial_route()
  let model = bright.init(model.init_state(), model.Computed)
  use model <- bright.start(model)
  model
  |> bright.update(update.handle_changed_route(_, route))
  |> bright.update(update.handle_submitted_search)
  |> bright.schedule(fn(_, _) { setup.modem() })
  |> bright.schedule(fn(data, _) { router.update_page_title(data.route) })
  |> bright.schedule(fn(_, _) { window.subscribe_focus() })
  |> bright.schedule(fn(_, _) { window.subscribe_is_mobile() })
  |> bright.schedule(fn(_, _) { api.get_trendings() })
  |> bright.schedule(fn(_, _) { api.get_packages() })
  |> bright.schedule(fn(_, _) { api.get_analytics() })
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  use model <- bright.start(model)
  bright.update(model, update_state(_, msg))
}

fn update_state(state: State, msg: Msg) {
  case msg {
    msg.ApiReturnedAnalytics(analytics:) ->
      update.handle_analytics(state, analytics)
    msg.ApiReturnedPackages(packages:) ->
      update.handle_packages(state, packages)
    msg.ApiReturnedSearchResults(input:, search_results:) ->
      update.handle_search_results(state, input, search_results)
    msg.ApiReturnedTrendings(trendings:) ->
      update.handle_trendings(state, trendings)
    msg.AppRequiredDiscussToast(message:) ->
      update.handle_discuss_toast(state, message)
    msg.BrowserChangedRoute(route:) -> update.handle_changed_route(state, route)
    msg.BrowserResizedViewport(is_mobile:) ->
      update.handle_resized_viewport(state, is_mobile)
    msg.UserClickedSidebarName(id:) ->
      update.handle_clicked_sidebar_name(state, id)
    msg.UserFocusedSearch(event:) -> update.handle_focused_search(state, event)
    msg.UserInputtedSearch(query:) ->
      update.handle_inputted_search(state, query)
    msg.UserPressedEscape -> update.handle_pressed_escape(state)
    msg.UserSubmittedSearch -> update.handle_submitted_search(state)
    msg.UserToggledFilter(filter:, value:) ->
      update.handle_toggle_filter(state, filter, value)
  }
}
