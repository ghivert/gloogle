import gleam/function
import gleam/string
import lustre/element/html as h

pub fn newline() {
  h.text("\n")
}

pub fn idt(indent: Int) {
  h.text(string.repeat(" ", indent))
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
