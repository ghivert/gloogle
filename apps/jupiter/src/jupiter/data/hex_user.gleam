import decrypt
import gleam/dynamic/decode
import gleam/option.{type Option}
import gleam/time/timestamp.{type Timestamp}

pub type HexUser {
  HexUser(
    id: String,
    username: String,
    email: Option(String),
    url: String,
    created_at: Timestamp,
    updated_at: Timestamp,
  )
}

pub fn decoder() {
  use id <- decode.field("id", decode.string)
  use username <- decode.field("username", decode.string)
  use email <- decode.field("email", decode.optional(decode.string))
  use url <- decode.field("url", decode.string)
  use created_at <- decode.field("created_at", decrypt.timestamp())
  use updated_at <- decode.field("updated_at", decrypt.timestamp())
  decode.success(HexUser(id:, username:, email:, url:, created_at:, updated_at:))
}
