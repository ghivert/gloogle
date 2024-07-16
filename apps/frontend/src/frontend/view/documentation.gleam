import gleam/list
import gleam/string
import lustre/attribute as a
import sketch/lustre/element as el

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
    |> converter()
    |> a.attribute("dangerous-unescaped-html", _)
  el.element("div", [content, a.class("documentation")], [], [])
}
