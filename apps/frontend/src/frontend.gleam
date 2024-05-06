import data/decoders/search_result
import data/model.{type Model}
import data/msg.{type Msg}
import frontend/view
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

  let apply_debugger = fn(fun: fn(a, tardis.Instance) -> a) {
    fn(app: a) {
      result.map(debugger_, fun(app, _))
      |> result.unwrap(app)
    }
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
    msg.UpdateInput(content) ->
      model
      |> model.update_input(content)
      |> update.none()
    msg.SubmitSearch -> {
      http.expect_json(search_result.decode_search_results, msg.SearchResults)
      |> http.get("http://localhost:3000/search?q=" <> model.input, _)
      |> pair.new(model, _)
    }
    msg.SearchResults(search_results) -> {
      let toast =
        search_results
        |> result.map_error(fn(error) {
          error
          |> toast_error.describe_http_error()
          |> option.map(toast.error)
        })
        |> result.unwrap_error(option.None)
        |> option.unwrap(effect.none())
      search_results
      |> result.map(model.update_search_results(model, _))
      |> result.unwrap(model)
      |> pair.new(toast)
    }
    msg.None -> update.none(model)
  }
}
