import backend/gleam/generate/types
import gleam/json
import gleam/list
import gleam/option.{type Option, Some}
import gleam/package_interface
import gleam/pair

pub fn generate(
  deprecation: Option(package_interface.Deprecation),
  impl: Option(package_interface.Implementations),
) {
  let deprecation =
    deprecation
    |> option.map(fn(d) { d.message })
    |> json.nullable(json.string)
    |> pair.new("deprecation", _)
  impl
  |> option.map(fn(i) { #("implementations", types.implementations_to_json(i)) })
  |> list.prepend([Some(deprecation)], _)
  |> option.values()
  |> json.object()
}
