import decrypt
import gleam/dynamic/decode
import gleam/time/timestamp.{type Timestamp}

pub type HexRead {
  HexRead(id: String, last_check: Timestamp)
}

pub fn decoder() {
  use id <- decode.field("id", decode.string)
  use last_check <- decode.field("last_check", decrypt.timestamp())
  decode.success(HexRead(id:, last_check:))
}
