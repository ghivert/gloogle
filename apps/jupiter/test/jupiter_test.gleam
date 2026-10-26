import data/ir
import data/ir/from_gleam
import gleam/list
import gleam/option.{None}
import gleam/package_interface as pi
import gleeunit
import jupiter/postgres/queries

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

// Phase-1 subset keys: every proper subset of the (sorted) query names as a
// canonical `,`-joined key — the empty set in, the full set out (`@>` covers
// it), each key in sorted order. 2^3 - 1 = 7 keys for three names.
pub fn subset_keys_test() {
  let keys = queries.subset_keys(["Bool", "Int", "List"])
  assert list.length(keys) == 7
  assert list.contains(keys, "")
  assert list.contains(keys, "Bool,List")
  assert list.contains(keys, "Int,List")
  assert !list.contains(keys, "Bool,Int,List")
  assert queries.subset_keys(["List"]) == [""]
  // A nameless query keeps only the empty key (its `@>` branch is off).
  assert queries.subset_keys([]) == [""]
  // Beyond six names the enumeration is skipped.
  assert queries.subset_keys(["A", "B", "C", "D", "E", "F", "G"]) == []
}
