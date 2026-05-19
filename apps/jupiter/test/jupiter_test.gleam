import envoy
import gleam/option.{Some}
import gleam/result
import gleeunit
import jupiter/context/postgres
import jupiter/gleam/parse
import jupiter/gleam/type_search/search
import pog

const signature = "fn use_callback(a, b) -> c"

const search_test = "fn (a, _) -> b"

pub fn main() {
  gleeunit.main()
}

fn postgres_connect() {
  let host = envoy.get("POSTGRES_HOST") |> result.unwrap("localhost")
  let assert Ok(_) =
    postgres.name()
    |> pog.default_config
    |> pog.host(host)
    |> pog.database("gloogle")
    |> pog.user("gloogle")
    |> pog.password(Some("gloogle"))
    |> pog.ssl(pog.SslDisabled)
    |> pog.start
  postgres.connection()
}

// gleeunit test functions end in `_test`
pub fn type_search_test() {
  let db = postgres_connect()
  let index = "1000"
  let assert Ok(kind) = parse.parse_function(signature)
  let assert Ok(skind) = parse.parse_function(search_test)
  let init = search.empty()
  let search = search.add(init, kind, index)
  assert Ok([index]) == search.find(search, skind, db)
}
