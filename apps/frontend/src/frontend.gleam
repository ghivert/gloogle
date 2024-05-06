import data/model.{type Model}
import data/msg.{type Msg}
import frontend/view
import gleam/dynamic
import gleam/io
import lustre
import lustre/effect
import lustre/update
import lustre_http as http
import sketch/lustre as sketch
import sketch/options as sketch_options
import tardis

pub fn main() {
  let init = fn(_) { #(model.init(), effect.none()) }

  let assert Ok(debugger_) = tardis.single("gloogle")

  let assert Ok(cache) =
    sketch_options.node()
    |> sketch.setup()

  let assert Ok(_) =
    view.view
    |> sketch.compose(cache)
    |> lustre.application(init, update, _)
    |> tardis.wrap(debugger_)
    |> lustre.start("#app", Nil)
    |> tardis.activate(debugger_)
}

fn update(model: Model, msg: Msg) {
  case msg {
    msg.UpdateInput(content) ->
      model
      |> model.update_input(content)
      |> update.none()
    msg.SubmitSearch -> {
      let input = model.input
      #(
        model,
        http.get(
          "http://localhost:3000/search?q=" <> input,
          http.expect_text(msg.SearchResults),
        ),
      )
    }
    msg.SearchResults(res) -> {
      io.debug(res)
      update.none(model)
    }
    msg.None -> update.none(model)
  }
}
