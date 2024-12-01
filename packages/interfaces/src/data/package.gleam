import gleam/dynamic
import gleam/json
import gleam/option.{type Option}
import gleam/result

pub type Package {
  Package(
    name: String,
    repository: Option(String),
    documentation: Option(String),
    hex_url: Option(String),
    licenses: List(String),
    description: Option(String),
    rank: Option(Int),
    popularity: Int,
  )
}

pub fn decode(dyn) {
  dynamic.decode8(
    Package,
    dynamic.field("name", dynamic.string),
    dynamic.field("repository", dynamic.optional(dynamic.string)),
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("hex_url", dynamic.optional(dynamic.string)),
    dynamic.field("licenses", fn(dyn) {
      dynamic.any([
        dynamic.list(dynamic.string),
        fn(dyn) {
          use data <- result.try(dynamic.optional(dynamic.string)(dyn))
          option.unwrap(data, "[]")
          |> json.decode(using: dynamic.list(dynamic.string))
          |> result.replace_error([dynamic.DecodeError("", "", [])])
        },
      ])(dyn)
    }),
    dynamic.field("description", dynamic.optional(dynamic.string)),
    dynamic.field("rank", dynamic.optional(dynamic.int)),
    dynamic.field("popularity", fn(dyn) {
      use data <- result.try(dynamic.optional(dynamic.string)(dyn))
      option.unwrap(data, "{}")
      |> json.decode(using: dynamic.optional_field("github", dynamic.int))
      |> result.map(option.unwrap(_, 0))
      |> result.replace_error([dynamic.DecodeError("", "", [])])
    }),
  )(dyn)
}

pub fn encode(package: Package) {
  json.object([
    #("name", json.string(package.name)),
    #("repository", json.nullable(package.repository, json.string)),
    #("documentation", json.nullable(package.documentation, json.string)),
    #("hex_url", json.nullable(package.hex_url, json.string)),
    #("licenses", json.array(package.licenses, json.string)),
    #("description", json.nullable(package.description, json.string)),
    #("rank", json.nullable(package.rank, json.int)),
    #("popularity", {
      json.object([#("github", json.int(package.popularity))])
      |> json.to_string
      |> json.string
    }),
  ])
}
