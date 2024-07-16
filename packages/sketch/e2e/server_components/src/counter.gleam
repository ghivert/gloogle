import gleam/int
import gleam/result
import lustre
import lustre/element/html
import lustre/event
import sketch
import sketch/lustre as sketch_lustre
import sketch/media
import sketch/options as sketch_options
import sketch/size.{px}

pub type Model =
  Int

pub type Msg {
  Increment
  Decrement
}

pub fn main() {
  let init = fn(_) { 0 }
  let assert Ok(cache) =
    sketch_options.node()
    |> sketch_lustre.setup()

  let assert Ok(_) =
    view
    |> sketch_lustre.compose(cache)
    |> lustre.simple(init, update, _)
    |> lustre.start("#app", Nil)
}

pub fn app() {
  let init = fn(_) { 0 }
  sketch_options.node()
  |> sketch_lustre.setup()
  |> result.map(fn(cache) {
    view
    |> sketch_lustre.compose(cache)
    |> lustre.simple(init, update, _)
  })
}

fn update(model: Model, msg: Msg) {
  case msg {
    Increment -> model + 1
    Decrement -> model - 1
  }
}

fn main_class() {
  sketch.class([
    sketch.background("red"),
    sketch.display("flex"),
    sketch.flex_direction("row"),
    sketch.gap(px(12)),
    sketch.padding(px(12)),
    sketch.hover([sketch.background("yellow")]),
    sketch.media(media.max_width(px(450)), [
      sketch.background("purple"),
      sketch.hover([sketch.background("white")]),
    ]),
  ])
  |> sketch.to_lustre()
}

fn color_class(model: Model) {
  let back = case model % 3 {
    0 -> "blue"
    _ -> "green"
  }
  let id = "color-" <> back
  sketch.dynamic(id, [sketch.background(back)])
  |> sketch.to_lustre()
}

fn button_class() {
  sketch.class([sketch.cursor("crosshair"), sketch.font_size_("14px")])
  |> sketch.to_lustre()
}

fn view(model: Model) {
  html.div([main_class()], [
    html.button([event.on_click(Decrement), button_class()], [
      html.text("Decrement"),
    ]),
    html.div([color_class(model)], [html.text(int.to_string(model))]),
    html.button([event.on_click(Increment), button_class()], [
      html.text("Increment"),
    ]),
  ])
}
