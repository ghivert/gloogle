import gleam/dynamic.{type Dynamic}
import gleam/pair
import lustre.{type Action, type App}
import lustre/effect.{type Effect}
import lustre/internals/runtime
import plinth/browser/document
import plinth/browser/element.{type Element}
import plinth/browser/shadow
import tardis/internals/data/msg.{type Msg}
import tardis/internals/stylesheet.{stylesheet}

pub type Middleware =
  fn(Dynamic, Dynamic) -> Nil

@external(javascript, "../../tardis.ffi.mjs", "addCustomStyles")
fn add_custom_styles(content: String) -> Nil

pub fn instanciate_shadow_root(element: Element) {
  // Instanciate the Shadow DOM wrapper.
  let div = document.create_element("div")
  let root = shadow.attach_shadow(div, shadow.Open)
  element.append_child(document.body(), div)
  element.set_attribute(div, "class", "tardis")
  shadow.append_child(root, element)
  root
}

pub fn instanciate_lustre_root() {
  // Instanciate the Shadow DOM lustre node.
  let root = document.create_element("div")
  element.set_attribute(root, "id", "tardis-start")
  root
}

pub fn mount_shadow_node() {
  // Add Lexend in the DOM.
  add_custom_styles(stylesheet)
  let lustre_root_ = instanciate_lustre_root()
  let shadow_root = instanciate_shadow_root(lustre_root_)
  // Trick to fool lustre application.
  // Please children, don't do this at home.
  let lustre_root: String = dynamic.unsafe_coerce(dynamic.from(lustre_root_))
  #(shadow_root, lustre_root)
}

pub fn wrap_init(middleware: Middleware) {
  fn(init) {
    fn(flags) {
      let new_state = init(flags)
      new_state
      |> pair.first()
      |> dynamic.from()
      |> middleware(dynamic.from("Init"))
      new_state
    }
  }
}

pub fn wrap_update(middleware: Middleware) {
  fn(update) {
    fn(model, msg) {
      let new_state = update(model, msg)
      new_state
      |> pair.first()
      |> dynamic.from()
      |> middleware(dynamic.from(msg))
      new_state
    }
  }
}

@external(javascript, "../../tardis.ffi.mjs", "updateLustre")
pub fn update_lustre(
  application: App(a, b, c),
  init_mapper: fn(fn(flags) -> #(model, Effect(msg))) ->
    fn(flags) -> #(model, Effect(msg)),
  update_mapper: fn(fn(model, msg) -> #(model, Effect(msg))) ->
    fn(model, msg) -> #(model, Effect(msg)),
) -> App(a, b, c)

pub fn create_model_updater(
  dispatch: fn(Action(Msg, c)) -> Nil,
  application: String,
) {
  fn(dispatcher: Dynamic) {
    fn(model: Dynamic) -> Effect(Msg) {
      effect.from(fn(_) {
        model
        |> dynamic.from()
        |> runtime.UpdateModel()
        |> dynamic.unsafe_coerce(dispatcher)
      })
    }
    |> msg.AddApplication(application, _)
    |> lustre.dispatch()
    |> dispatch()
  }
}

pub fn step_adder(
  dispatch: fn(Action(Msg, lustre.ClientSpa)) -> Nil,
  name: String,
) {
  fn(model, msg) {
    model
    |> msg.AddStep(name, _, msg)
    |> lustre.dispatch()
    |> dispatch()
  }
}
