import data/ir
import gleam/json
import gleam/option.{None, Some}

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

// `fn(b, a) -> a` with arbitrary source ids (5, 3) → first-appearance 0, 1.
// Makes alpha-equivalent signatures identical.
pub fn alpha_rename_test() {
  let var = fn(id, name) { ir.Param(None, False, ir.Var(id, name)) }
  let sig = ir.Fn([var(5, "x"), var(3, "y")], ir.Var(3, "y"), False)
  let expected = ir.Fn([var(0, "x"), var(1, "y")], ir.Var(1, "y"), False)
  assert ir.alpha_rename(sig) == expected
  assert ir.to_string(ir.alpha_rename(sig)) == "($0, $1) -> $1"
}

// A `TypeDefinition`'s constructors share one variable numbering (the variables
// are bound once at the type head). `type T(a, b) { First(a) Second(b, a) }` with
// arbitrary source ids: reading left-to-right First's `a`→$0, Second's `b`→$1,
// and Second's `a` reuses $0.
pub fn alpha_rename_declaration_test() {
  let param = fn(id, name) { ir.Param(None, False, ir.Var(id, name)) }
  let definition =
    ir.TypeDefinition(parameters: 2, constructors: [
      ir.Constructor("First", [param(5, "a")]),
      ir.Constructor("Second", [param(3, "b"), param(5, "a")]),
    ])
  let expected =
    ir.TypeDefinition(parameters: 2, constructors: [
      ir.Constructor("First", [param(0, "a")]),
      ir.Constructor("Second", [param(1, "b"), param(0, "a")]),
    ])
  assert ir.alpha_rename_declaration(definition) == expected
}

// Fingerprint of `result.map`: `fn(Result(a, b), fn(a) -> c) -> Result(c, b)`.
// arity = 2 (top-level params); `type_names` = the concrete nominal names,
// nesting flattened, `Var` contributing nothing, `Result` deduped despite two
// occurrences.
pub fn fingerprint_test() {
  let result_ = fn(a, b) {
    ir.Named(ir.Name("gleam_stdlib", "gleam/result", "Result", None), [a, b])
  }
  let mapper =
    ir.Fn([ir.Param(None, False, ir.Var(0, "a"))], ir.Var(2, "c"), False)
  let declaration =
    ir.Function(ir.Fn(
      params: [
        ir.Param(None, False, result_(ir.Var(0, "a"), ir.Var(1, "b"))),
        ir.Param(None, False, mapper),
      ],
      return: result_(ir.Var(2, "c"), ir.Var(1, "b")),
      is_method: False,
    ))
  assert ir.fingerprint(declaration) == #(2, ["Result"])
}

// `type_names` is canonical: deduplicated and sorted, whatever order the names
// appear in. `fn(String, Int) -> Bool` mentions them as String, Int, Bool and
// stores Bool, Int, String — so equal name sets are equal arrays, which the
// phase-1 subset lookup compares by btree equality.
pub fn fingerprint_names_are_sorted_test() {
  let named = fn(name) { ir.Named(ir.Name("", "gleam", name, None), []) }
  let declaration =
    ir.Function(ir.Fn(
      params: [
        ir.Param(None, False, named("String")),
        ir.Param(None, False, named("Int")),
      ],
      return: named("Bool"),
      is_method: False,
    ))
  assert ir.fingerprint(declaration) == #(2, ["Bool", "Int", "String"])
}

// A `TypeAlias` fingerprints through its aliased type, like `Constant`: an alias
// of a function type carries that function's arity, so phase 1 (`arity = $1`)
// keeps it for the queries phase 2 (`unify_declaration`, which matches against
// `aliased`) would verify. An alias of a non-function type stays at arity 0.
pub fn fingerprint_type_alias_test() {
  let int = ir.Named(ir.Name("gleam_stdlib", "gleam", "Int", None), [])
  let bool = ir.Named(ir.Name("gleam_stdlib", "gleam", "Bool", None), [])
  let callback =
    ir.TypeAlias(
      parameters: 0,
      aliased: ir.Fn([ir.Param(None, False, int)], bool, False),
    )
  assert ir.fingerprint(callback) == #(1, ["Bool", "Int"])
  let pair = ir.TypeAlias(parameters: 0, aliased: ir.Tuple([int, bool]))
  assert ir.fingerprint(pair) == #(0, ["Bool", "Int"])
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

// ─── Normal form (`to_canonical`) ────────────────────────────────────────────

fn named(name: String, args: List(ir.Type)) -> ir.Type {
  ir.Named(ir.Name("", "", name, None), args)
}

fn param(type_: ir.Type) -> ir.Param {
  ir.Param(None, False, type_)
}

// The four liberties the unifier takes collapse to one string: union order,
// method form, labels, module qualification.
pub fn canonical_normalises_test() {
  let int = named("Int", [])
  let str = named("String", [])
  // Union members are a set.
  assert ir.to_canonical(ir.Union([int, str]))
    == ir.to_canonical(ir.Union([str, int]))
  assert ir.to_canonical(ir.Union([str, int])) == "Int | String"
  // A method and its function form are one type.
  let method = ir.Fn([param(int), param(str)], str, True)
  let function = ir.Fn([param(int), param(str)], str, False)
  assert ir.to_string(method) == "Int :: (String) -> String"
  assert ir.to_canonical(method) == "(Int, String) -> String"
  assert ir.to_canonical(method) == ir.to_canonical(function)
  // Labels are dropped.
  let labelled = ir.Fn([ir.Param(Some("with"), False, int)], str, False)
  assert ir.to_string(labelled) == "(with: Int) -> String"
  assert ir.to_canonical(labelled) == "(Int) -> String"
  // The module qualification is dropped; the written form keeps it.
  let option =
    ir.Named(ir.Name("gleam_stdlib", "gleam/option", "Option", None), [int])
  assert ir.to_string(option) == "gleam/option.Option<Int>"
  assert ir.to_canonical(option) == "Option<Int>"
  // Record fields are sorted by name.
  let record = ir.Record([ir.Field("b", str), ir.Field("a", int)])
  assert ir.to_canonical(record) == "{ a: Int, b: String }"
}

// Alpha-equivalent types share one spelling, whatever their source ids, and
// variables are numbered in the normalised reading order.
pub fn canonical_alpha_test() {
  let var = fn(id, name) { param(ir.Var(id, name)) }
  let a = ir.Fn([var(5, "x"), var(3, "y")], ir.Var(3, "y"), False)
  let b = ir.Fn([var(0, "a"), var(1, "b")], ir.Var(1, "b"), False)
  assert ir.to_canonical(a) == "($0, $1) -> $1"
  assert ir.to_canonical(a) == ir.to_canonical(b)
  // Union sorting is variable-blind: `List<a> | Int` and `Int | List<b>` meet.
  let list_of = fn(t) { named("List", [t]) }
  let u1 = ir.Union([list_of(ir.Var(9, "a")), named("Int", [])])
  let u2 = ir.Union([named("Int", []), list_of(ir.Var(2, "b"))])
  assert ir.to_canonical(u1) == ir.to_canonical(u2)
  assert ir.to_canonical(u1) == "Int | List<$0>"
  // `to_canonical` is idempotent through parse-free re-entry: normalising a
  // normal form changes nothing.
  assert ir.to_canonical(ir.alpha_rename(a)) == ir.to_canonical(a)
}

// A declaration's normal form is the type the unifier matches; a
// `TypeDefinition` has none.
pub fn canonical_declaration_test() {
  let int = named("Int", [])
  assert ir.canonical(ir.Function(ir.Fn([param(int)], int, False)))
    == Some("(Int) -> Int")
  assert ir.canonical(ir.Constant(int)) == Some("Int")
  assert ir.canonical(ir.TypeAlias(0, int)) == Some("Int")
  assert ir.canonical(ir.TypeDefinition(0, [])) == None
}
