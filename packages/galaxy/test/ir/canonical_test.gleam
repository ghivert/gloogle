import data/ir
import data/ir/canonical
import gleam/option.{None, Some}

fn named(name: String, args: List(ir.Type)) -> ir.Type {
  ir.Named(ir.Name("", "", name, None), args)
}

fn param(type_: ir.Type) -> ir.Param {
  ir.Param(None, False, type_)
}

fn var(index: Int) -> ir.Type {
  ir.Var(index, "")
}

// Every production of the notation, read as printed.
pub fn canonical_parse_test() {
  assert canonical.parse("Int") == Ok(named("Int", []))
  assert canonical.parse("$3") == Ok(var(3))
  assert canonical.parse("_") == Ok(ir.Hole)
  assert canonical.parse("Result<$0, $1>")
    == Ok(named("Result", [var(0), var(1)]))
  assert canonical.parse("gleam/option.Option<Int>")
    == Ok(
      ir.Named(ir.Name("", "gleam/option", "Option", None), [named("Int", [])]),
    )
  assert canonical.parse("#(Int, $0)")
    == Ok(ir.Tuple([named("Int", []), var(0)]))
  assert canonical.parse("(Int, $0) -> $0")
    == Ok(ir.Fn([param(named("Int", [])), param(var(0))], var(0), False))
  assert canonical.parse("() -> Int") == Ok(ir.Fn([], named("Int", []), False))
  assert canonical.parse("(with: Int, ...$0) -> Bool")
    == Ok(ir.Fn(
      [
        ir.Param(Some("with"), False, named("Int", [])),
        ir.Param(None, True, var(0)),
      ],
      named("Bool", []),
      False,
    ))
  assert canonical.parse("Int :: (String) -> String")
    == Ok(ir.Fn(
      [param(named("Int", [])), param(named("String", []))],
      named("String", []),
      True,
    ))
  assert canonical.parse("{ a: Int, b: $0 }")
    == Ok(ir.Record([ir.Field("a", named("Int", [])), ir.Field("b", var(0))]))
  assert canonical.parse("Int | String")
    == Ok(ir.Union([named("Int", []), named("String", [])]))
}

// The printer's parentheses resolve what the bare notation leaves ambiguous:
// a function in a union, a union returned, a composite receiver.
pub fn canonical_grouping_test() {
  let int = named("Int", [])
  let str = named("String", [])
  let fn_ = ir.Fn([param(int)], str, False)
  let union = ir.Union([int, str])
  // A function nested in a union.
  let member = ir.Union([fn_, int])
  assert ir.to_string(member) == "((Int) -> String) | Int"
  assert canonical.parse(ir.to_string(member)) == Ok(member)
  // A union returned by a function.
  let returned = ir.Fn([param(int)], union, False)
  assert ir.to_string(returned) == "(Int) -> (Int | String)"
  assert canonical.parse(ir.to_string(returned)) == Ok(returned)
  // Bare, a return extends as far as it can: the same text without the
  // parentheses is a function returning the union.
  assert canonical.parse("(Int) -> Int | String") == Ok(returned)
  // A receiver keeps its label and spread marker.
  let labelled =
    ir.Fn([ir.Param(Some("self"), True, int), param(int)], str, True)
  assert ir.to_string(labelled) == "(self: ...Int) :: (Int) -> String"
  assert canonical.parse(ir.to_string(labelled)) == Ok(labelled)
  // A receiver that is a union.
  let method = ir.Fn([param(union), param(int)], str, True)
  assert ir.to_string(method) == "(Int | String) :: (Int) -> String"
  assert canonical.parse(ir.to_string(method)) == Ok(method)
}

// Variables are read with their index and no source name: that is all the
// notation carries, and the matcher reads the index alone.
pub fn canonical_var_name_test() {
  let assert Ok(ir.Var(5, name)) = canonical.parse("$5")
  assert name == ""
}

// What the grammar refuses: a bare `$`, trailing input, a duplicate record
// field, an unbalanced argument list.
pub fn canonical_rejects_test() {
  assert canonical.parse("$") == Error(Nil)
  assert canonical.parse("Int junk") == Error(Nil)
  assert canonical.parse("{ a: Int, a: String }") == Error(Nil)
  assert canonical.parse("List<Int") == Error(Nil)
  assert canonical.parse("") == Error(Nil)
}

// Duplicate union members collapse — the set the unifier already assumes.
pub fn canonical_union_dedupe_test() {
  assert canonical.parse("Int | Int") == Ok(named("Int", []))
  assert canonical.parse("Int | String | Int")
    == Ok(ir.Union([named("Int", []), named("String", [])]))
}
