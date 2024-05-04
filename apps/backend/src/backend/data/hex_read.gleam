import birl.{type Time}
import gleam/dynamic
import helpers

pub type HexRead {
  HexRead(id: Int, last_check: Time)
}

pub fn decode(data) {
  dynamic.decode2(
    HexRead,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, helpers.decode_time),
  )(data)
}
