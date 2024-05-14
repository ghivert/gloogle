import data/model.{type Model}
import data/msg.{type Msg}
import data/search_result
import frontend/view
import gleam/bool
import gleam/option
import gleam/pair
import gleam/result
import grille_pain
import grille_pain/lustre/toast
import grille_pain/options
import lustre
import lustre/effect
import lustre/update
import lustre_http as http
import sketch/lustre as sketch
import sketch/options as sketch_options
import tardis
import toast/error as toast_error

@external(javascript, "./config.ffi.mjs", "is_dev")
fn is_dev() -> Bool

@external(javascript, "./config.ffi.mjs", "scrollTo")
fn scroll_to_element(id: String) -> Nil

pub fn main() {
  let init = fn(_) { #(model.init(), effect.none()) }

  let debugger_ = case is_dev() {
    False -> Error(Nil)
    True ->
      tardis.single("Gloogle")
      |> result.nil_error()
  }

  let assert Ok(cache) =
    sketch_options.node()
    |> sketch.setup()

  let assert Ok(_) =
    options.default()
    |> options.timeout(5000)
    |> grille_pain.setup()

  let apply_debugger = fn(app: a, fun: fn(a, tardis.Instance) -> a) {
    result.map(debugger_, fun(app, _))
    |> result.unwrap(app)
  }

  let assert Ok(_) =
    view.view
    |> sketch.compose(cache)
    |> lustre.application(init, update, _)
    |> apply_debugger(tardis.wrap)
    |> lustre.start("#app", Nil)
    |> apply_debugger(tardis.activate)
}

fn update(model: Model, msg: Msg) {
  case msg {
    msg.UpdateInput(content) -> update_input(model, content)
    msg.SubmitSearch -> submit_search(model)
    msg.Reset -> reset(model)
    msg.None -> update.none(model)
    msg.ScrollTo(id) -> scroll_to(model, id)
    msg.SearchResults(search_results) ->
      handle_search_results(model, search_results)
  }
}

fn update_input(model: Model, content: String) {
  model
  |> model.update_input(content)
  |> update.none()
}

fn reset(model: Model) {
  model
  |> model.reset()
  |> update.none()
}

fn submit_search(model: Model) {
  let endpoint = case is_dev() {
    True -> "http://localhost:3000"
    False -> "https://api.gloogle.run"
  }
  use <- bool.guard(when: model.input == "", return: #(model, effect.none()))
  http.expect_json(search_result.decode_search_results, msg.SearchResults)
  |> http.get(endpoint <> "/search?q=" <> model.input, _)
  |> pair.new(model, _)
}

fn scroll_to(model: Model, id: String) {
  let eff = effect.from(fn(_dispatch) { scroll_to_element(id) })
  #(model, eff)
}

fn handle_search_results(
  model: Model,
  search_results: Result(search_result.SearchResults, http.HttpError),
) {
  let toast = display_toast(search_results)
  search_results
  |> result.map(model.update_search_results(model, _))
  |> result.unwrap(model)
  |> pair.new(toast)
}

fn display_toast(
  search_results: Result(search_result.SearchResults, http.HttpError),
) {
  search_results
  |> result.map_error(fn(error) {
    toast_error.describe_http_error(error)
    |> option.map(toast.error)
  })
  |> result.unwrap_error(option.None)
  |> option.unwrap(effect.none())
}
