import envoy
import gleam/result
import jupiter/context/environment.{type Environment}
import jupiter/context/postgres
import pog
import wisp

pub type Context {
  Context(
    db: pog.Connection,
    hex_api_key: String,
    github_token: String,
    env: Environment,
  )
}

pub fn init() {
  let env = environment.read()
  use hex_api_key <- result.try(envoy.get("HEX_API_KEY"))
  use github_token <- result.map(envoy.get("GITHUB_TOKEN"))
  let db = postgres.connection()
  Context(db:, hex_api_key:, github_token:, env:)
}

pub fn get_secret_key_base() {
  wisp.random_string(64)
}
