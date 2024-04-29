import birl.{type Time}
import gleam/option.{type Option}

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
