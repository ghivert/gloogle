import data/ir
import data/ir/from_gleam
import envoy
import gleam/json
import gleam/option.{None, Some}
import gleam/package_interface as pi
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

// `result.map`: `fn(Result(a, b), fn(a) -> c) -> Result(c, b)` — the canonical
// form Gleam and TS front-ends must both produce (see signature-ir.md).
pub fn ir_print_result_map_test() {
  let result_ = fn(a, b) { ir.Named(ir.Name("", "", "Result", None), [a, b]) }
  let mapper =
    ir.Fn([ir.Param(None, False, ir.Var(0, "a"))], ir.Var(2, "c"), False)
  let map =
    ir.Fn(
      [
        ir.Param(None, False, result_(ir.Var(0, "a"), ir.Var(1, "b"))),
        ir.Param(None, False, mapper),
      ],
      result_(ir.Var(2, "c"), ir.Var(1, "b")),
      False,
    )
  assert ir.to_string(map) == "(Result<$0, $1>, ($0) -> $2) -> Result<$2, $1>"
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

// `fn(b, a) -> a` with arbitrary source ids (5, 3) → first-appearance 0, 1.
// Makes alpha-equivalent signatures identical.
pub fn alpha_rename_test() {
  let var = fn(id, name) { ir.Param(None, False, ir.Var(id, name)) }
  let sig = ir.Fn([var(5, "x"), var(3, "y")], ir.Var(3, "y"), False)
  let expected = ir.Fn([var(0, "x"), var(1, "y")], ir.Var(1, "y"), False)
  assert ir.alpha_rename(sig) == expected
  assert ir.to_string(ir.alpha_rename(sig)) == "($0, $1) -> $1"
}

// The IR JSON codec round-trips: decode(encode(d)) == d. Exercises Named (with
// concept), Var, nested Fn, and labelled/unlabelled Params.
pub fn codec_round_trip_test() {
  let result_ = fn(a, b) {
    ir.Named(ir.Name("gleam_stdlib", "gleam/result", "Result", None), [a, b])
  }
  let mapper =
    ir.Fn([ir.Param(None, False, ir.Var(0, "a"))], ir.Var(2, "c"), False)
  let declaration =
    ir.Function(ir.Fn(
      params: [
        ir.Param(None, False, result_(ir.Var(0, "a"), ir.Var(1, "b"))),
        ir.Param(Some("with"), False, mapper),
      ],
      return: result_(ir.Var(2, "c"), ir.Var(1, "b")),
      is_method: False,
    ))
  let encoded = ir.encode(declaration) |> json.to_string
  assert json.parse(encoded, ir.decoder()) == Ok(declaration)
}
