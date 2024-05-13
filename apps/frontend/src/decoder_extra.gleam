import gleam/dynamic.{type Dynamic}
import gleam/option
import gleam/result

pub fn completely_option(field: String) {
  fn(dyn: Dynamic) {
    dynamic.optional_field(field, dynamic.optional(dynamic.string))(dyn)
    |> result.map(fn(res) { option.flatten(res) })
  }
}
