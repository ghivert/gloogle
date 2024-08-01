import backend/gleam/parse.{type Kind, Function}
import backend/gleam/type_search
import gleam/function
import gleam/int
import gleam/io
import gleam/result
import gleeunit
import gleeunit/should
import pprint

pub fn main() {
  gleeunit.main()
}

const signature = "fn decode14(
  fn(a, b, c, d, e, f, g, h, i, j, k, l, m, n) -> o,
  #(a, b),
  fn(gleam/dynamic.Dynamic) -> Result(a, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(b, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(c, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(d, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(e, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(f, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(g, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(h, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(i, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(j, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(k, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(l, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(m, List(gleam/dynamic.DecodeError)),
  fn(gleam/dynamic.Dynamic) -> Result(n, List(gleam/dynamic.DecodeError))
) -> fn(gleam/dynamic.Dynamic) -> Result(o, List(gleam/dynamic.DecodeError))"

// gleeunit test functions end in `_test`
pub fn type_search_test() {
  let index = int.random(1000)
  use kind <- result.try(parse.parse_function(signature) |> result.nil_error)
  let search = type_search.add(type_search.empty(), kind, index)
  type_search.find(search, kind)
  |> function.tap(should.equal(_, Ok([index])))
}
