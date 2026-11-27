import gleam/list
import gleam/string
import lustre/attribute as a
import lustre/element

@external(javascript, "../../markdown.ffi.mjs", "convert")
fn converter(content: String) -> String

fn remove_leading_space(str: String) {
  case str {
    " " <> rest -> rest
    str -> str
  }
}

pub fn view(document: String) {
  let content =
    document
    |> string.split("\n")
    |> list.map(remove_leading_space)
    |> string.join("\n")
    |> converter
  // The HTML is sanitised by DOMPurify in `markdown.ffi.mjs`.
  element.unsafe_raw_html("", "div", [a.class("documentation")], content)
}
