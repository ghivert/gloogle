pub type Component

@external(javascript, "./main.ffi.mjs", "jsx")
@internal
pub fn jsx(value: a, props: props, children: b) -> Component

@external(javascript, "./main.ffi.mjs", "setFunctionName")
fn set_function_name(a: a, name: String) -> a

@external(javascript, "./main.ffi.mjs", "addProxy")
fn add_proxy(
  a: fn(props, List(Component)) -> Component,
) -> fn(props, List(Component)) -> Component

pub fn component(
  name: String,
  val: fn(props, List(Component)) -> Component,
) -> fn(props, List(Component)) -> Component {
  let val = add_proxy(val)
  set_function_name(val, name)
  fn(props, children) { jsx(val, props, children) }
}

pub fn component_(name: String, val: fn(props) -> Component) {
  set_function_name(val, name)
  fn(props) { jsx(val, props, Nil) }
}

@external(javascript, "./main.ffi.mjs", "strictMode")
pub fn strict_mode(children: List(Component)) -> Component

@external(javascript, "./main.ffi.mjs", "fragment")
pub fn fragment(children: List(Component)) -> Component

pub fn keyed(
  element: fn(List(Component)) -> Component,
  content: List(#(String, Component)),
) {
  content
  |> coerce
  |> element
}

@external(javascript, "./main.ffi.mjs", "coerce")
fn coerce(a: a) -> b
