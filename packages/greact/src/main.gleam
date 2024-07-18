import gleam/dynamic
import gleam/int
import gleam/io
import react

pub type Root

pub type Node(props)

pub type Children

pub fn main() {
  let root = root()
  create_root("root")
  |> render(root([]))
  // |> render(children())
}

pub fn app() {
  div([], [a([], [img([])]), text("Meh?")])
}

pub fn keyed(
  f: fn(List(Component)) -> Component,
  content: List(#(String, Component)),
) {
  f(dynamic.unsafe_coerce(dynamic.from(content)))
}

pub fn a(attrs, children: List(Component)) -> Component {
  jsx("a", Nil, children)
}

pub fn img(attrs) -> Component {
  jsx("img", Nil, [])
}

pub fn text(content) -> Component {
  jsx("text_", Nil, content)
}

@external(javascript, "./main.ffi.mjs", "setFunctionName")
fn set_function_name(a: a, name: String) -> a

@external(javascript, "react", "useState")
fn use_state(default: a) -> #(a, fn(a) -> Nil)

@external(javascript, "react", "useState")
fn use_state_(default: a) -> #(a, fn(fn(a) -> a) -> Nil)

@external(javascript, "./main.ffi.mjs", "useTimeout")
fn use_timeout(default: fn() -> Nil) -> Nil

@external(javascript, "./main.ffi.mjs", "useHelloEffect")
fn use_hello_effect(deps: List(a)) -> Nil

pub fn root() {
  let inside = mk_inside()
  use props <- component("Root")
  let #(state, set_state) = use_state_(0)
  use_timeout(fn() {
    use state <- set_state()
    state + 1
  })
  use_hello_effect([])
  strict_mode([app(), inside(InsideProps(state))])
}

pub type InsideProps {
  InsideProps(count: Int)
}

pub fn mk_inside() {
  let inside = mk_inside_help()
  use props: InsideProps <- component("Insider")
  react.div([], [
    inside(props),
    react.div([], [react.text("inside " <> int.to_string(props.count))]),
  ])
}

pub fn mk_inside_help() {
  use props: InsideProps <- component("InsiderHelp")
  div([], [text("inside " <> int.to_string(props.count))])
}
