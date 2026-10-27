import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, Some}
import gleam/package_interface as pi
import gleam/pair

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
  |> option.map(fn(i) { #("implementations", implementations_to_json(i)) })
  |> list.prepend([Some(deprecation)], _)
  |> option.values
  |> json.object
}

fn implementations_to_json(implementations: pi.Implementations) -> Json {
  let uses_js = json.bool(implementations.uses_javascript_externals)
  json.object([
    #("gleam", json.bool(implementations.gleam)),
    #("uses_erlang_externals", json.bool(implementations.uses_erlang_externals)),
    #("uses_javascript_externals", uses_js),
  ])
}
