import gleam/coerce.{coerce}
import gleam/dict
import lustre
import lustre/attribute
import lustre/effect
import lustre/element.{type Element}
import lustre/event

@external(javascript, "../config.ffi.mjs", "coerceEvent")
fn coerce_event(a: a) -> b

const tag_name = "lazy-node"

type Model(msg) =
  Element(msg)

type Msg(msg) {
  UpdateContent(Model(msg))
  OnContentMessage(msg)
}

fn update(model: Model(msg), msg: Msg(msg)) {
  case msg {
    UpdateContent(content) -> #(content, effect.none())
    OnContentMessage(msg) -> #(model, event.emit("msg", coerce(msg)))
  }
}

fn on_attribute_change() {
  let update_content = fn(dyn) { Ok(UpdateContent(coerce(dyn))) }
  dict.from_list([#("content", update_content)])
}

pub fn setup() {
  let init = fn(_) { #(element.none(), effect.none()) }
  let view = element.map(_, OnContentMessage)
  on_attribute_change()
  |> lustre.component(init, update, view, _)
  |> lustre.register(tag_name)
}

fn on_coerce(value: a) {
  Ok(coerce_event(value))
}

pub fn lazy(content: Element(msg)) -> Element(msg) {
  let content = attribute.property("content", content)
  element.element(tag_name, [content, event.on("msg", on_coerce)], [])
}
