import data/implementations.{type Implementations, Implementations}
import gleam/decoder_extra
import gleam/dynamic
import gleam/json
import gleam/option.{type Option}

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
  []
  |> fn(elems) {
    case metadata.deprecation {
      option.None -> elems
      option.Some(deprecation) -> [#("deprecation", json.string(deprecation))]
    }
  }
  |> fn(elems) {
    case metadata.implementations {
      option.None -> elems
      option.Some(implementations) -> [
        #(
          "implementations",
          json.object([
            #("gleam", json.bool(implementations.gleam)),
            #(
              "uses_erlang_externals",
              json.bool(implementations.uses_erlang_externals),
            ),
            #(
              "uses_javascript_externals",
              json.bool(implementations.uses_javascript_externals),
            ),
          ]),
        ),
      ]
    }
  }
  |> json.object
}
