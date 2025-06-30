import backend/gleam/type_search/msg as type_search
import backend/postgres/postgres
import envoy
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option, None}
import gleam/result
import pog
import wisp

pub type Environment {
  Development
  Production
}

pub type Context {
  Context(
    db: pog.Connection,
    hex_api_key: String,
    github_token: String,
    env: Environment,
    type_search_subject: Option(Subject(type_search.Msg)),
  )
}

pub fn init() {
  let env = read_environment()
  use database_url <- result.try(envoy.get("DATABASE_URL"))
  use hex_api_key <- result.try(envoy.get("HEX_API_KEY"))
  use github_token <- result.map(envoy.get("GITHUB_TOKEN"))
  let db = postgres.connect(database_url)
  Context(db:, hex_api_key:, github_token:, env:, type_search_subject: None)
}

pub fn read_environment() {
  case envoy.get("GLEAM_ENV") {
    Ok("development") -> Development
    _ -> Production
  }
}

pub fn get_secret_key_base() {
  wisp.random_string(64)
}

pub fn is_dev() {
  envoy.get("GLEAM_ENV") == Ok("development")
}

pub fn bucket_uri() {
  envoy.get("BUCKET_URI")
}

pub fn scaleway_keys() {
  use access_key <- result.try(envoy.get("SCALEWAY_ACCESS_KEY"))
  use secret_key <- result.map(envoy.get("SCALEWAY_SECRET_KEY"))
  #(access_key, secret_key)
}
