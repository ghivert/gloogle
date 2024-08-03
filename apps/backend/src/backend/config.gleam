import backend/gleam/type_search/msg as type_search
import gleam/erlang/os
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/option.{type Option}
import gleam/pgo
import gleam/result
import gleam/string
import wisp
import wisp/logger

pub type Environment {
  Development
  Production
}

pub type Context {
  Context(
    db: pgo.Connection,
    hex_api_key: String,
    github_token: String,
    env: Environment,
    type_search_subject: Option(Subject(type_search.Msg)),
  )
}

pub type Config {
  Config(
    database_url: String,
    hex_api_key: String,
    port: Int,
    level: logger.Level,
    github_token: String,
    env: Environment,
  )
}

pub fn read_config() {
  let assert Ok(database_url) = os.get_env("DATABASE_URL")
  let assert Ok(hex_api_key) = os.get_env("HEX_API_KEY")
  let assert Ok(github_token) = os.get_env("GITHUB_TOKEN")
  let env = case result.unwrap(os.get_env("GLEAM_ENV"), "") {
    "development" -> Development
    _ -> Production
  }
  let assert Ok(port) =
    os.get_env("PORT")
    |> result.try(int.parse)
  let level =
    os.get_env("LOG_LEVEL")
    |> result.try(logger.parse)
    |> result.unwrap(logger.Info)
  Config(database_url, hex_api_key, port, level, github_token, env)
}

pub fn get_secret_key_base() {
  wisp.random_string(64)
}

pub fn is_dev() {
  os.get_env("GLEAM_ENV") == Ok("development")
}

pub fn bucket_uri() {
  os.get_env("BUCKET_URI")
}

pub fn scaleway_keys() {
  use access_key <- result.try(os.get_env("SCALEWAY_ACCESS_KEY"))
  use secret_key <- result.map(os.get_env("SCALEWAY_SECRET_KEY"))
  #(access_key, secret_key)
}

pub fn add_type_search_subject(context, subject) {
  Context(..context, type_search_subject: option.Some(subject))
}
