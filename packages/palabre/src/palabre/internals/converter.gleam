import gleam/dict
import gleam/dynamic
import gleam/list
import gleam/string
import palabre/internals/utils

pub fn format_fields(fields: List(#(String, List(String)))) {
  fields
  |> list.map(fn(field) {
    let #(key, vals) = field
    case utils.is_color() {
      False -> key <> "=" <> vals |> list.reverse |> string.join(",")
      True -> {
        let vals = vals |> list.reverse |> string.join("\u{1b}[33m,\u{1b}[0m")
        let key = "\u{1b}[32m" <> key
        let equal = "\u{1b}[33m" <> "="
        let vals = "\u{1b}[0m" <> vals
        key <> equal <> vals
      }
    }
  })
  |> string.join(" ")
}

pub fn format_json(fields: List(#(String, List(String))), text: String) {
  let init = dict.from_list([#("message", dynamic.from(text))])
  use acc, #(key, values) <- list.fold(fields, init)
  case values {
    [value] -> dict.insert(acc, key, dynamic.from(value))
    values -> dict.insert(acc, key, dynamic.from(values))
  }
}
