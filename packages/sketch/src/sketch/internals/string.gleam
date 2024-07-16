import gleam/option.{type Option}
import gleam/string

pub fn indent(indent: Int) {
  string.repeat(" ", indent)
}

pub fn wrap_class(
  id: String,
  properties: List(String),
  idt: Int,
  pseudo: Option(String),
) {
  let base_indent = indent(idt)
  let pseudo_ = option.unwrap(pseudo, "")
  [base_indent <> "." <> id <> pseudo_ <> " {", ..properties]
  |> string.join("\n")
  |> string.append("\n" <> base_indent <> "}")
}
