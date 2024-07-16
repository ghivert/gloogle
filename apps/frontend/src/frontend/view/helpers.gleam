import gleam/function
import gleam/string
import sketch/lustre/element as el

pub fn newline() {
  el.text("\n")
}

pub fn idt(indent: Int) {
  el.text(string.repeat(" ", indent))
}

pub fn hexdocs_link(
  package package: String,
  version version: String,
  module module: String,
  name name: String,
) {
  let join = function.flip(string.join)
  let base = join("/", ["https://hexdocs.pm", package, version, module])
  base <> ".html#" <> name
}
