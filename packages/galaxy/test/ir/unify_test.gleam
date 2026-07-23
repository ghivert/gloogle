import data/ir
import data/ir/unify.{Equivalent, Subsumed}
import gleam/list
import gleam/option.{None, Some}

fn named(name: String, args: List(ir.Type)) -> ir.Type {
  ir.Named(ir.Name("", "", name, None), args)
}

fn param(type_: ir.Type) -> ir.Param {
  ir.Param(None, False, type_)
}

fn var(index: Int) -> ir.Type {
  ir.Var(index, "a")
}

fn function(params: List(ir.Type), return: ir.Type) -> ir.Type {
  ir.Fn(params: list.map(params, param), return:, is_method: False)
}

// The identity case: an alpha-renamed query is literally the stored IR — a
// hole-free exact match, cost 0.
pub fn identical_test() {
  let map =
    function(
      [named("List", [var(0)]), function([var(0)], var(1))],
      named("List", [var(1)]),
    )
  assert unify.unify(map, map) == Ok(Equivalent(0))
}

// `Var` binding is injective in both directions. `($0, $1) -> $0` is
// alpha-equivalent to `fn(a, b) -> a`; to `fn(a, b) -> b` only modulo argument
// order (the permutation surcharge); and to `fn(a, a) -> a` not at all — the
// two variables collapse onto one, which is subsumption, not equivalence.
pub fn var_injective_test() {
  let query = function([var(0), var(1)], var(0))
  assert unify.unify(query, function([var(0), var(1)], var(0)))
    == Ok(Equivalent(0))
  assert unify.unify(query, function([var(0), var(1)], var(1)))
    == Ok(Equivalent(1))
  assert unify.unify(query, function([var(0), var(0)], var(0)))
    == Ok(Subsumed(0))
}

// A candidate variable is not free either: `($0, $0) -> $0` against
// `fn(a, b) -> a` fails alpha-equivalence (the mapping would stop being
// injective backwards) but the candidate *is* more general (substitute
// `b := a`), so it subsumes — with variable-to-variable bindings, cost 0.
pub fn var_injective_backwards_test() {
  let query = function([var(0), var(0)], var(0))
  assert unify.unify(query, function([var(0), var(1)], var(0)))
    == Ok(Subsumed(0))
}

// `Hole` matches any type, at any depth, binds nothing, and costs what it
// absorbs: nothing for a variable, the node count for concrete structure.
pub fn hole_test() {
  let query = function([named("List", [ir.Hole])], ir.Hole)
  assert unify.unify(
      query,
      function([named("List", [named("Int", [])])], named("String", [])),
    )
    == Ok(Equivalent(2))
  assert unify.unify(query, function([named("List", [var(0)])], var(1)))
    == Ok(Equivalent(0))
  // The nesting still has to line up for equivalence: a bare `a` is not a
  // `List<_>` — but it is more general than one, so it subsumes (absorbing
  // the `List` node, cost 1).
  assert unify.unify(query, function([var(0)], var(1))) == Ok(Subsumed(1))
}

// A query `Var` is not a wildcard: equivalence demands a candidate variable.
// Searching `($0) -> $0` puts the identity first; `Int -> Int` is a
// specialization, one bucket down.
pub fn var_is_not_a_hole_test() {
  let query = function([var(0)], var(0))
  let int = named("Int", [])
  assert unify.unify(query, function([var(0)], var(0))) == Ok(Equivalent(0))
  assert unify.unify(query, function([int], int)) == Ok(Subsumed(1))
  // Consistency survives: the *same* query variable cannot be two types at
  // once, in any tier.
  assert unify.unify(query, function([int], named("String", []))) == Error(Nil)
}

// Nominal names must match, and arity of the application too: `List<$0>` is
// not `Set<$0>`, and `List<$0>` is not `List`.
pub fn named_test() {
  let query = function([named("List", [var(0)])], var(0))
  assert unify.unify(query, function([named("Set", [var(0)])], var(0)))
    == Error(Nil)
  assert unify.unify(query, function([named("List", [])], var(0))) == Error(Nil)
}

// Parameter order is a priced rewrite, not an option: positional first, then
// reordered with the surcharge.
pub fn permutation_surcharge_test() {
  let query =
    function([named("Int", []), named("String", [])], named("Bool", []))
  let same =
    function([named("Int", []), named("String", [])], named("Bool", []))
  let swapped =
    function([named("String", []), named("Int", [])], named("Bool", []))
  assert unify.unify(query, same) == Ok(Equivalent(0))
  assert unify.unify(query, swapped) == Ok(Equivalent(1))
}

// Permutation backtracks *through the return type*: the identity pairing of
// parameters succeeds here, but leaves the return unmatchable — the search
// must undo it and try the other pairing.
pub fn permutation_backtracks_test() {
  let list_of = fn(t) { named("List", [t]) }
  let query = function([ir.Hole, list_of(var(0))], var(0))
  let candidate = function([list_of(var(0)), list_of(var(1))], var(0))
  assert unify.unify(query, candidate) == Ok(Equivalent(2))
}

// A method and the equivalent pipe-first function are the same shape — that
// convergence is why methods lower receiver-first.
pub fn is_method_ignored_test() {
  let params = [param(named("List", [var(0)])), param(named("Int", []))]
  let method = ir.Fn(params:, return: var(0), is_method: True)
  let function = ir.Fn(params:, return: var(0), is_method: False)
  assert unify.unify(method, function) == Ok(Equivalent(0))
  assert unify.unify(function, method) == Ok(Equivalent(0))
}

// A label written in the query constrains; an omitted one does not, but the
// unlabelled candidate is the closer match (the label surcharge).
pub fn labels_test() {
  let labelled = fn(label) {
    ir.Fn([ir.Param(label, False, named("Int", []))], named("Bool", []), False)
  }
  assert unify.unify(labelled(None), labelled(None)) == Ok(Equivalent(0))
  assert unify.unify(labelled(None), labelled(Some("with")))
    == Ok(Equivalent(1))
  assert unify.unify(labelled(Some("with")), labelled(Some("with")))
    == Ok(Equivalent(0))
  assert unify.unify(labelled(Some("with")), labelled(Some("by"))) == Error(Nil)
  assert unify.unify(labelled(Some("with")), labelled(None)) == Error(Nil)
}

// `A | B` ≡ `B | A`: union members match as a set, without a surcharge.
pub fn union_test() {
  let union = fn(members) { ir.Union(members) }
  let query = union([named("Int", []), named("String", [])])
  assert unify.unify(query, union([named("String", []), named("Int", [])]))
    == Ok(Equivalent(0))
  assert unify.unify(query, union([named("String", []), named("Bool", [])]))
    == Error(Nil)
}

// Records match by field name, and exactly: an extra candidate field is
// structural subtyping, which is deferred.
pub fn record_test() {
  let record = fn(fields) { ir.Record(fields) }
  let query =
    record([ir.Field("id", var(0)), ir.Field("name", named("String", []))])
  assert unify.unify(
      query,
      record([ir.Field("name", named("String", [])), ir.Field("id", var(0))]),
    )
    == Ok(Equivalent(0))
  assert unify.unify(
      query,
      record([
        ir.Field("id", var(0)),
        ir.Field("name", named("String", [])),
        ir.Field("age", named("Int", [])),
      ]),
    )
    == Error(Nil)
  assert unify.unify(
      query,
      record([ir.Field("id", var(0)), ir.Field("label", named("String", []))]),
    )
    == Error(Nil)
}

// Tuples are positional, unlike unions and records — and stay positional even
// under permutation, which reorders parameters only.
pub fn tuple_test() {
  let query = ir.Tuple([named("Int", []), named("String", [])])
  assert unify.unify(query, ir.Tuple([named("Int", []), named("String", [])]))
    == Ok(Equivalent(0))
  assert unify.unify(query, ir.Tuple([named("String", []), named("Int", [])]))
    == Error(Nil)
}

// ─── Subsumption ────────────────────────────────────────────────────────────

// One direction: the query is more general, its variables bind to concrete
// candidate types and the cost is the structure they absorb.
pub fn query_more_general_test() {
  let query = function([var(0)], var(0))
  let int = named("Int", [])
  assert unify.unify(query, function([int], int)) == Ok(Subsumed(1))
}

// The other direction, and the real recall win: the candidate is more general
// than the query. The *closer* generalization costs less — `fn(List(a)) -> Int`
// absorbed only `Int`, `fn(a) -> Int` absorbed the whole `List(Int)`.
pub fn candidate_more_general_ranking_test() {
  let int = named("Int", [])
  let query = function([named("List", [int])], int)
  assert unify.unify(query, function([named("List", [var(0)])], int))
    == Ok(Subsumed(1))
  assert unify.unify(query, function([var(0)], int)) == Ok(Subsumed(2))
}

// Within the `Equivalent` bucket, holes rank by how much they absorbed: the
// genuinely polymorphic candidate first, then small concrete, then big.
pub fn equivalent_ranking_test() {
  let query = function([ir.Hole], named("Int", []))
  let int = named("Int", [])
  assert unify.unify(query, function([var(0)], int)) == Ok(Equivalent(0))
  assert unify.unify(query, function([int], int)) == Ok(Equivalent(1))
  assert unify.unify(
      query,
      function([named("List", [named("Set", [named("String", [])])])], int),
    )
    == Ok(Equivalent(3))
}

// The line subsumption stops at: `(a, a) -> Int` and `(a, Int) -> Int` share a
// common instance (`(Int, Int) -> Int`) but neither is more general — rejected.
// Only the isolated symmetric unifier accepts.
pub fn common_instance_rejected_test() {
  let int = named("Int", [])
  let query = function([var(0), var(0)], int)
  let candidate = function([var(0), int], int)
  assert unify.unify(query, candidate) == Error(Nil)
  assert unify.unify_symmetric(query, candidate, False)
}

// ─── Level 3 (isolated) — symmetric unification ─────────────────────────────

// The canonical common-instance pair: `#(a, Int)` and `#(Bool, b)` unify on
// `#(Bool, Int)`, yet neither can serve the other. The cascade drops it; the
// isolated symmetric unifier accepts it.
pub fn symmetric_common_instance_test() {
  let query = ir.Tuple([var(0), named("Int", [])])
  let candidate = ir.Tuple([named("Bool", []), var(1)])
  assert unify.unify(query, candidate) == Error(Nil)
  assert unify.unify_symmetric(query, candidate, False)
}

// The occurs check: `#($0, List<$0>)` against `#(List<$1>, $1)` would build the
// infinite type `List<List<…>>`. It must fail, not hang.
pub fn symmetric_occurs_check_test() {
  let list_of = fn(t) { named("List", [t]) }
  let query = ir.Tuple([var(0), list_of(var(0))])
  let candidate = ir.Tuple([list_of(var(1)), var(1)])
  assert !unify.unify_symmetric(query, candidate, False)
}

// The stored side is a `Declaration`: functions, constants and aliases expose a
// type to match; a type definition has none.
pub fn declaration_test() {
  let type_ = named("Int", [])
  assert unify.unify_declaration(
      function([type_], type_),
      ir.Function(function([type_], type_)),
    )
    == Ok(Equivalent(0))
  assert unify.unify_declaration(type_, ir.Constant(type_)) == Ok(Equivalent(0))
  assert unify.unify_declaration(
      type_,
      ir.TypeAlias(parameters: 0, aliased: type_),
    )
    == Ok(Equivalent(0))
  assert unify.unify_declaration(
      type_,
      ir.TypeDefinition(parameters: 0, constructors: [ir.Constructor("A", [])]),
    )
    == Error(Nil)
}
