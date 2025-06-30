import gleam/dynamic/decode.{type Decoder}
import gleam/json
import gleam/option.{type Option}
import gleam/time/timestamp

pub fn optional(
  field: String,
  decoder: Decoder(a),
  next: fn(Option(a)) -> Decoder(b),
) -> Decoder(b) {
  decode.optional_field(field, option.None, decode.optional(decoder), next)
}

pub fn timestamp() {
  use date <- decode.then(decode.string)
  case timestamp.parse_rfc3339(date) {
    Ok(timestamp) -> decode.success(timestamp)
    Error(_) -> decode.failure(timestamp.system_time(), "Timestamp")
  }
}

pub fn json(decoder: Decoder(a)) {
  decode.one_of(decoder, or: [
    decode.then(decode.string, fn(data) {
      case json.parse(data, decoder) {
        Ok(data) -> decode.success(data)
        Error(_) -> decoder
      }
    }),
  ])
}
