import backend/gleam/parse
import backend/gleam/type_search
import gleam/function
import gleam/int
import gleam/io
import gleam/result
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

const signature = "fn use_callback(a, b) -> c"

const search_test = "fn (a, _) -> b"

// gleeunit test functions end in `_test`
pub fn type_search_test() {
  let index = int.random(1000)
  use kind <- result.try(parse.parse_function(signature) |> result.nil_error)
  use skind <- result.try(parse.parse_function(search_test) |> result.nil_error)
  let search = type_search.add(type_search.empty(), kind, index)
  Ok(search)
  // type_search.find(search, skind)
  // |> function.tap(should.equal(_, Ok([index])))
}
