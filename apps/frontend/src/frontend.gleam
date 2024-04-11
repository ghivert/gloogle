import data/model.{type Model}
import data/msg.{type Msg}
import lustre
import lustre/effect
import lustre/element/html as h
import lustre/update
import sketch/lustre as sketch
import sketch/options as sketch_options
import tardis

pub fn main() {
  let assert Ok(debugger_) = tardis.singleton("gling")

  let assert Ok(cache) =
    sketch_options.document()
    |> sketch.setup()

  let assert Ok(_) =
    fn(_) { #(model.init(), effect.none()) }
    |> lustre.application(update, view)
    |> sketch.wrap(cache)
    |> tardis.wrap(debugger_)
    |> lustre.start("#app", Nil)
    |> tardis.activate(debugger_)
}

fn update(model: Model, msg: Msg) {
  case msg {
    msg.None -> update.none(model)
  }
}

fn view(_model: Model) {
  h.div([], [])
}
