import birl.{type Time}
import gleam/dynamic
import gleam/option.{type Option}
import helpers

pub type HexUser {
  HexUser(
    id: Int,
    username: String,
    email: Option(String),
    url: String,
    created_at: Time,
    updated_at: Time,
  )
}

pub fn decode(data) {
  dynamic.decode6(
    HexUser,
    dynamic.element(0, dynamic.int),
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.optional(dynamic.string)),
    dynamic.element(3, dynamic.string),
    dynamic.element(4, helpers.decode_time),
    dynamic.element(5, helpers.decode_time),
  )(data)
}
