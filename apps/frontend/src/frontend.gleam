import bright
import data/model.{type Model}
import data/msg.{type Msg}
import frontend/effects/api
import frontend/effects/window
import frontend/router
import frontend/setup
import frontend/update
import frontend/view
import lustre/effect

pub fn main() {
  let assert Ok(_) = setup.sketch()
  let assert Ok(_) = setup.components()
  let assert Ok(_) = setup.grille_pain()
  let assert Ok(_) = setup.start_application(init, update, view.view)
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  let route = setup.initial_route()
  let model = bright.init(model.init_data(), model.Computed)
  use model <- bright.start(model)
  use model <- bright.update(model, update.handle_changed_route(_, route))
  use model <- bright.update(model, update.handle_submitted_search)
  model
  |> bright.run(fn(_, _) { setup.modem() })
  |> bright.run(fn(data, _) { router.update_page_title(data.route) })
  |> bright.run(fn(_, _) { window.subscribe_focus() })
  |> bright.run(fn(_, _) { window.subscribe_is_mobile() })
  |> bright.run(fn(_, _) { api.get_trendings() })
  |> bright.run(fn(_, _) { api.get_packages() })
  |> bright.run(fn(_, _) { api.get_analytics() })
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
