import decrypt
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option}

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

pub fn decoder() {
  use name <- decode.field("decode", decode.string)
  use repository <- decode.field("repository", decode.optional(decode.string))
  let documentation = decode.optional(decode.string)
  use documentation <- decode.field("documentation", documentation)
  use hex_url <- decode.field("hex_url", decode.optional(decode.string))
  let licenses = decode.one_of(decode.list(decode.string), [license_decoder()])
  use licenses <- decode.field("licenses", licenses)
  use description <- decode.field("description", decode.optional(decode.string))
  use rank <- decode.field("rank", decode.optional(decode.int))
  use popularity <- decode.field("popularity", {
    decode.optional({
      decrypt.json({ decrypt.optional("github", decode.int, decode.success) })
    })
  })
  let popularity = option.flatten(popularity) |> option.unwrap(0)
  decode.success({
    Package(
      name:,
      repository:,
      documentation:,
      hex_url:,
      licenses:,
      description:,
      rank:,
      popularity:,
    )
  })
}

fn license_decoder() {
  use data <- decode.then(decode.optional(decode.string))
  let data = option.unwrap(data, "[]")
  case json.parse(data, using: decode.list(decode.string)) {
    Error(_) -> decode.failure([], "License")
    Ok(licenses) -> decode.success(licenses)
  }
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
