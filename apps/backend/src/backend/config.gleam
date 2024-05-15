import gleam/erlang/os
import gleam/int
import gleam/pgo
import gleam/result
import wisp
import wisp/logger

pub type Context {
  Context(db: pgo.Connection, hex_api_key: String)
}

pub type Config {
  Config(
    database_url: String,
    hex_api_key: String,
    port: Int,
    level: logger.Level,
  )
}

pub fn read_config() {
  let assert Ok(database_url) = os.get_env("DATABASE_URL")
  let assert Ok(hex_api_key) = os.get_env("HEX_API_KEY")
  let assert Ok(port) =
    os.get_env("PORT")
    |> result.try(int.parse)
  let level =
    os.get_env("LOG_LEVEL")
    |> result.try(logger.parse)
    |> result.unwrap(logger.Info)
  Config(database_url, hex_api_key, port, level)
}

pub fn get_secret_key_base() {
  wisp.random_string(64)
}

pub fn is_dev() {
  os.get_env("GLEAM_ENV") == Ok("development")
}

pub fn bucket_uri() {
  let assert Ok(content) = os.get_env("BUCKET_URI")
  content
}

pub fn scaleway_keys() {
  let assert Ok(access_key) = os.get_env("SCALEWAY_ACCESS_KEY")
  let assert Ok(secret_key) = os.get_env("SCALEWAY_SECRET_KEY")
  #(access_key, secret_key)
}
