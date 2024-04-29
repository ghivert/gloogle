import gleam/erlang/os
import gleam/pgo
import wisp

pub type Context {
  Context(connection: pgo.Connection)
}

pub type Config {
  Config(database_url: String, hex_api_key: String)
}

pub fn read_config() {
  let assert Ok(database_url) = os.get_env("DATABASE_URL")
  let assert Ok(hex_api_key) = os.get_env("HEX_API_KEY")
  Config(database_url, hex_api_key)
}

pub fn get_secret_key_base() {
  wisp.random_string(64)
}

pub fn is_dev() {
  os.get_env("GLEAM_ENV") == Ok("development")
}
