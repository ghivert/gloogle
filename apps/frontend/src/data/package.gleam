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

pub fn decoder(dyn) {
  dynamic.decode8(
    Package,
    dynamic.field("name", dynamic.string),
    dynamic.field("repository", dynamic.optional(dynamic.string)),
    dynamic.field("documentation", dynamic.optional(dynamic.string)),
    dynamic.field("hex-url", dynamic.optional(dynamic.string)),
    dynamic.field("licenses", fn(dyn) {
      use data <- result.try(dynamic.optional(dynamic.string)(dyn))
      option.unwrap(data, "[]")
      |> json.decode(using: dynamic.list(dynamic.string))
      |> result.replace_error([dynamic.DecodeError("", "", [])])
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
