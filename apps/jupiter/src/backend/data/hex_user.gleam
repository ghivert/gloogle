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
    dynamic.field("id", dynamic.int),
    dynamic.field("username", dynamic.string),
    dynamic.field("email", dynamic.optional(dynamic.string)),
    dynamic.field("url", dynamic.string),
    dynamic.field("created_at", helpers.decode_time),
    dynamic.field("updated_at", helpers.decode_time),
  )(data)
}
