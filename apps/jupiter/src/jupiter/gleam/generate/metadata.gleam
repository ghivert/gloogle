import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, Some}
import gleam/package_interface as pi
import gleam/pair
import jupiter/gleam/generate/types

pub fn generate(
  deprecation: Option(pi.Deprecation),
  impl: Option(pi.Implementations),
) -> Json {
  let deprecation =
    deprecation
    |> option.map(fn(d) { d.message })
    |> json.nullable(json.string)
    |> pair.new("deprecation", _)
  impl
  |> option.map(fn(i) { #("implementations", types.implementations_to_json(i)) })
  |> list.prepend([Some(deprecation)], _)
  |> option.values
  |> json.object
}
