import data/implementations.{type Implementations, Implementations}
import gleam/decoder_extra
import gleam/dynamic
import gleam/json
import gleam/option.{type Option}
import gleam/pair

pub type Metadata {
  Metadata(
    deprecation: Option(String),
    implementations: Option(Implementations),
  )
}

pub fn decode(dyn) {
  dynamic.decode2(
    Metadata,
    decoder_extra.completely_option("deprecation"),
    dynamic.optional_field(
      "implementations",
      dynamic.decode3(
        Implementations,
        dynamic.field("gleam", dynamic.bool),
        dynamic.field("uses_erlang_externals", dynamic.bool),
        dynamic.field("uses_javascript_externals", dynamic.bool),
      ),
    ),
  )(dyn)
}

pub fn encode(metadata: Metadata) {
  let Metadata(deprecation:, implementations:) = metadata
  []
  |> append_optional(deprecation, fn(d) { #("deprecation", json.string(d)) })
  |> append_optional(implementations, encode_implementations)
  |> json.object
}

fn encode_implementations(i: Implementations) {
  json.object([
    #("gleam", json.bool(i.gleam)),
    #("uses_erlang_externals", json.bool(i.uses_erlang_externals)),
    #("uses_javascript_externals", json.bool(i.uses_javascript_externals)),
  ])
  |> pair.new("implementations", _)
}

fn append_optional(elems, data, mapper) {
  case data {
    option.None -> elems
    option.Some(data) -> [mapper(data), ..elems]
  }
}
