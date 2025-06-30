import backend/gleam/parse
import backend/gleam/type_search
import envoy
import gleam/function
import gleam/int
import gleam/option
import gleam/result
import gleeunit
import gleeunit/should
import pog

const signature = "fn use_callback(a, b) -> c"

const search_test = "fn (a, _) -> b"

pub fn main() {
  gleeunit.main()
}

fn postgres_connect() {
  let host = envoy.get("POSTGRES_HOST") |> result.unwrap("localhost")
  pog.Config(
    ..pog.default_config(),
    host: host,
    database: "gloogle",
    user: "gloogle",
    password: option.Some("gloogle"),
    ssl: False,
  )
  |> pog.connect
}

// gleeunit test functions end in `_test`
pub fn type_search_test() {
  let db = postgres_connect()
  let index = int.random(1000)
  use kind <- result.try({
    parse.parse_function(signature)
    |> result.replace_error(Nil)
  })
  use skind <- result.try({
    parse.parse_function(search_test)
    |> result.replace_error(Nil)
  })
  let search = type_search.add(type_search.empty(), kind, index)
  type_search.find(search, skind, db)
  |> function.tap(should.equal(_, Ok([index])))
}
