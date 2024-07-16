import gleam/int
import lustre
import lustre/element
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

fn second_class() {
  sketch.class([
    sketch.background("green"),
    sketch.font_size(px(20)),
    sketch.font_family("-apple-system"),
  ])
  |> sketch.memo()
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
  sketch.class([sketch.cursor("crosshair"), sketch.font_size(px(14))])
  |> sketch.to_lustre()
}

fn class_test(model: Model) {
  case model % 5 {
    0 -> html.div([second_class()], [html.text("Class Test")])
    _ -> element.none()
  }
}

fn view(model: Model) {
  html.div([], [
    html.div([main_class()], [
      html.button([event.on_click(Decrement), button_class()], [
        html.text("Decrement"),
      ]),
      html.div([color_class(model)], [html.text(int.to_string(model))]),
      html.button([event.on_click(Increment), button_class()], [
        html.text("Increment"),
      ]),
    ]),
    class_test(model),
    class_test(model),
    class_test(model),
  ])
}
