import backend/data/hex_read
import backend/data/hex_user
import backend/index/helpers
import gleam/dynamic

pub fn hex_user(data) {
  dynamic.decode6(
    hex_user.HexUser,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.optional(dynamic.string)),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, helpers.decode_time),
    dynamic.element(5, helpers.decode_time),
  )(data)
}

pub fn hex_read(data) {
  dynamic.decode2(
    hex_read.HexRead,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, helpers.decode_time),
  )(data)
}
