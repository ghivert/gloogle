import birl.{type Time}
import gleam/dynamic
import helpers

pub type HexRead {
  HexRead(id: Int, last_check: Time)
}

pub fn decode(data) {
  dynamic.decode2(
    HexRead,
    dynamic.field("id", dynamic.int),
    dynamic.field("last_check", helpers.decode_time),
  )(data)
}
