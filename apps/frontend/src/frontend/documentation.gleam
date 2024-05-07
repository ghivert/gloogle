import frontend/documentation/styles as s
import gleam/io
import gleam/list
import lustre/attribute as a
import lustre/element
import lustre/element/html as h

@external(javascript, "../markdown.ffi.mjs", "convert")
fn converter(content: String) -> String

pub fn view(document: String) {
  let content =
    document
    |> converter()
  h.div([a.attribute("dangerous-unescaped-html", content)], [])
}
