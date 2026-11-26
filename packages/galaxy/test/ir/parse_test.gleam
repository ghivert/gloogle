import data/ir
import data/ir/parse
import gleam/list
import gleam/option.{None, Some}

fn named(name: String, args: List(ir.Type)) -> ir.Type {
  ir.Named(ir.Name("", "", name, None), args)
}

fn qualified(module: String, name: String, args: List(ir.Type)) -> ir.Type {
  ir.Named(ir.Name("", module, name, None), args)
}

fn param(type_: ir.Type) -> ir.Param {
  ir.Param(None, False, type_)
}

fn function(params: List(ir.Type), return: ir.Type) -> ir.Type {
  ir.Fn(params: list.map(params, param), return:, is_method: False)
}

fn var(index: Int, name: String) -> ir.Type {
  ir.Var(index, name)
}

// A bare nominal with no arguments.
pub fn bare_named_test() {
  assert parse.parse("Int") == Ok(named("Int", []))
}

// A type variable alpha-renames to index 0, keeping its source name.
pub fn bare_var_test() {
  assert parse.parse("a") == Ok(var(0, "a"))
}

// `_` is the query wildcard.
pub fn hole_test() {
  assert parse.parse("_") == Ok(ir.Hole)
}

// Nominal application uses parentheses in Gleam.
pub fn applied_named_test() {
  assert parse.parse("List(a)") == Ok(named("List", [var(0, "a")]))
}

pub fn nested_named_test() {
  assert parse.parse("Result(a, b)")
    == Ok(named("Result", [var(0, "a"), var(1, "b")]))
}

// A qualified name keeps its module; the base name is what the matcher compares.
pub fn qualified_named_test() {
  assert parse.parse("option.Option(a)")
    == Ok(qualified("option", "Option", [var(0, "a")]))
}

pub fn deep_qualified_named_test() {
  assert parse.parse("gleam/option.Option(a)")
    == Ok(qualified("gleam/option", "Option", [var(0, "a")]))
}

pub fn tuple_test() {
  assert parse.parse("#(Int, String)")
    == Ok(ir.Tuple([named("Int", []), named("String", [])]))
}

// The canonical result.map worked example from `signature-ir.md`.
pub fn function_test() {
  let expected =
    function(
      [
        named("Result", [var(0, "a"), var(1, "b")]),
        function([var(0, "a")], var(2, "c")),
      ],
      named("Result", [var(2, "c"), var(1, "b")]),
    )
  assert parse.parse("fn(Result(a, b), fn(a) -> c) -> Result(c, b)")
    == Ok(expected)
}

// A nullary function.
pub fn nullary_function_test() {
  assert parse.parse("fn() -> a") == Ok(function([], var(0, "a")))
}

// Repeated variables share an index; that is what distinguishes them from holes.
pub fn repeated_var_test() {
  assert parse.parse("fn(a, a) -> a")
    == Ok(function([var(0, "a"), var(0, "a")], var(0, "a")))
}

// Each `_` is an independent wildcard, never unified with another.
pub fn independent_holes_test() {
  assert parse.parse("fn(_, _) -> _")
    == Ok(function([ir.Hole, ir.Hole], ir.Hole))
}

// First-appearance numbering: reading order is params left-to-right, then return.
pub fn first_appearance_numbering_test() {
  assert parse.parse("fn(b, a) -> b")
    == Ok(function([var(0, "b"), var(1, "a")], var(0, "b")))
}

// A labelled parameter keeps its label.
pub fn labelled_param_test() {
  assert parse.parse("fn(over: List(a)) -> Int")
    == Ok(ir.Fn(
      params: [ir.Param(Some("over"), False, named("List", [var(0, "a")]))],
      return: named("Int", []),
      is_method: False,
    ))
}

// Whitespace between tokens is irrelevant.
pub fn whitespace_test() {
  assert parse.parse("fn(  Int ,String )->Bool")
    == Ok(function([named("Int", []), named("String", [])], named("Bool", [])))
}

// A higher-order argument is itself a function type.
pub fn higher_order_test() {
  assert parse.parse("fn(fn(a) -> b) -> b")
    == Ok(function([function([var(0, "a")], var(1, "b"))], var(1, "b")))
}

pub fn garbage_is_error_test() {
  assert parse.parse("fn(") == Error(Nil)
}

// Trailing tokens are rejected, not silently dropped.
pub fn trailing_input_is_error_test() {
  assert parse.parse("List(a) junk") == Error(Nil)
}
