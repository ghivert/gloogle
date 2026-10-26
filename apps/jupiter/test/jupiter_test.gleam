import data/ir
import data/ir/from_gleam
import gleam/option.{None}
import gleam/package_interface as pi
import gleeunit

pub fn main() {
  gleeunit.main()
}

// `Result(a, Int)` from `gleam/result`, with a prelude `Int`. Checks package +
// qualifier extraction from the module, prelude staying unqualified, `Variable`
// → `Var` (pi ids preserved, alpha-renaming is a later pass), and nesting.
pub fn lower_type_test() {
  let pi_type =
    pi.Named("Result", "gleam_stdlib", "gleam/result", [
      pi.Variable(0),
      pi.Named("Int", "", "gleam", []),
    ])
  let expected =
    ir.Named(ir.Name("gleam_stdlib", "gleam/result", "Result", None), [
      ir.Var(0, "a"),
      ir.Named(ir.Name("", "", "Int", None), []),
    ])
  assert from_gleam.type_(pi_type) == expected
}
