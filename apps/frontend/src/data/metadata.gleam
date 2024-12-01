import data/implementations.{type Implementations, Implementations}
import gleam/decoder_extra
import gleam/dynamic
import gleam/option.{type Option}

pub type Metadata {
  Metadata(
    deprecation: Option(String),
    implementations: Option(Implementations),
  )
}

pub fn decode_metadata(dyn) {
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
