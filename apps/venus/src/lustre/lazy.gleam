import gleam/dynamic/decode
import gleam/unsafe
import lustre
import lustre/attribute
import lustre/component
import lustre/effect
import lustre/element.{type Element}
import lustre/event

const tag_name = "lazy-node"

type Model(msg) =
  Element(msg)

type Msg(msg) {
  UpdateContent(Model(msg))
  OnContentMessage(msg)
}

pub fn lazy(content: Element(msg)) -> Element(msg) {
  let content = unsafe.coerce(content)
  let content = attribute.property("content", content)
  let on_msg = event.on("msg", extract_detail())
  element.element(tag_name, [content, on_msg], [])
}

pub fn setup() {
  let init = fn(_) { #(element.none(), effect.none()) }
  let view = element.map(_, OnContentMessage)
  lustre.component(init, update, view, [
    component.on_property_change("content", {
      decode.dynamic
      |> decode.map(unsafe.coerce)
      |> decode.map(UpdateContent)
    }),
  ])
  |> lustre.register(tag_name)
}

fn update(model: Model(msg), msg: Msg(msg)) {
  case msg {
    UpdateContent(content) -> #(content, effect.none())
    OnContentMessage(msg) -> #(model, event.emit("msg", unsafe.coerce(msg)))
  }
}

fn extract_detail() {
  use detail <- decode.field("detail", decode.dynamic)
  let detail = unsafe.coerce(detail)
  decode.success(detail)
}
