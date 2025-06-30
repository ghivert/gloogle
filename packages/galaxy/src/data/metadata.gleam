import data/implementations.{type Implementations}
import decrypt
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option}

pub type Metadata {
  Metadata(
    deprecation: Option(String),
    implementations: Option(Implementations),
  )
}

pub fn decoder() {
  use deprecation <- decrypt.optional("deprecation", decode.string)
  let implementations = implementations.decoder()
  use implementations <- decrypt.optional("implementations", implementations)
  decode.success(Metadata(deprecation:, implementations:))
}

pub fn encode(metadata: Metadata) {
  let Metadata(deprecation:, implementations:) = metadata
  json.object([
    #("deprecation", json.nullable(deprecation, json.string)),
    #("implementations", {
      use i <- json.nullable(implementations)
      json.object([
        #("gleam", json.bool(i.gleam)),
        #("uses_erlang_externals", json.bool(i.uses_erlang_externals)),
        #("uses_javascript_externals", json.bool(i.uses_javascript_externals)),
      ])
    }),
  ])
}
