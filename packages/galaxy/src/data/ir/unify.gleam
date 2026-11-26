//// Verify — phase 2 of type search. Phase 1 narrows the table to a few hundred
//// candidates with the GIN fingerprint (`arity` + `type_names`); this module
//// decides, per candidate, *how well* it matches the query. Both sides are the
//// alpha-renamed IR: the query comes from `data/ir/parse`, the candidate is the
//// stored `signature_ir` decoded with `ir.decoder()`.
////
//// The verdict is a `Match`, not a boolean — the Hoogle lesson: matching
//// returns a cost, and the cost is the ranking. Bucket by constructor, rank by
//// `cost` inside a bucket:
////
////   - `Equivalent(cost)` — alpha-equivalence. A query `Var` matches a
////     candidate `Var` and nothing else, under a mapping injective both ways.
////     `cost` is what the query's `Hole`s absorbed (0 for a hole-free exact
////     match), plus surcharges (labels, permutation).
////   - `Subsumed(cost)` — directional subsumption: one side is more general
////     than the other. `($0) -> $0` finds `Int -> Int` (query more general)
////     and `(List<Int>) -> Bool` finds `fn(List(a)) -> Bool` (candidate more
////     general). Implemented as one-sided matching run both ways; `cost` is
////     the concrete structure the bindings absorbed, cheapest direction wins.
////
//// Costs measure the liberties taken to bridge the two sides: every concrete
//// node absorbed by a hole or a variable binding weighs 1 (`weight`), an
//// omitted label costs `label_cost`, a parameter reordering costs
//// `permutation_cost`. First match wins — the cost is the first successful
//// derivation's, not a proven minimum: good enough to rank, cheap to compute.
////
//// Argument permutation is not an option but a priced rewrite (Hoogle prices
//// argument reordering the same way): positional order is tried first,
//// reordering second with the surcharge, and skipped beyond
//// `permutation_limit` parameters (the search is factorial).
////
//// `Hole` is the wildcard: it matches anything and binds nothing, which is
//// what separates it from `Var`. A once-occurring `Var` behaves like a `Hole`
//// under subsumption (nothing to stay consistent with); the difference bites
//// only on repeated variables and under `Equivalent`.
////
//// Subsumption deliberately stops short of full unification: it rejects pairs
//// that merely share a common instance without either being more general (e.g.
//// `#(a, Int)` and `#(Bool, b)`). That last step lives in `unify_symmetric`,
//// isolated and not wired into the cascade — see its doc. We can afford the
//// tighter tier because `_` lets the user ask for looseness precisely, so the
//// blunt common-instance match is never the only way to get a wanted result.
////
//// The remaining tolerant equivalences (name synonyms via `Name.concept`,
//// wrapper transparency, subtyping) are layered on later — see
//// `signature-ir-todos.md`.

import data/ir.{type Declaration, type Field, type Param, type Type}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/order.{type Order}
import gleam/result

/// The verdict, per candidate. Bucket by constructor — `Equivalent` results sit
/// above `Subsumed` ones — and rank by `cost` inside a bucket, lower = closer.
pub type Match {
  Equivalent(cost: Int)
  Subsumed(cost: Int)
}

/// Rank two verdicts for result ordering: every `Equivalent` sits above every
/// `Subsumed`, and inside a bucket the lower `cost` (the closer match) comes
/// first. Pass to `list.sort` over the verified candidates.
pub fn compare(a: Match, b: Match) -> Order {
  case a, b {
    Equivalent(x), Equivalent(y) -> int.compare(x, y)
    Subsumed(x), Subsumed(y) -> int.compare(x, y)
    Equivalent(_), Subsumed(_) -> order.Lt
    Subsumed(_), Equivalent(_) -> order.Gt
  }
}

/// Flat surcharge when the parameters had to be reordered to match.
const permutation_cost = 1

/// Cost of a candidate label the query did not write.
const label_cost = 1

/// Beyond this many parameters, reordering is skipped and matching stays
/// positional: the search is factorial, and no real signature both has that
/// many parameters and needs reordering to be found.
const permutation_limit = 6

/// Which term a variable came from. The two namespaces are disjoint: query `$0`
/// and candidate `$0` are different variables, both alpha-renamed from 0 in
/// their own term.
type Side {
  Query
  Candidate
}

pub fn unify(query: Type, candidate: Type) -> Result(Match, Nil) {
  exact(#(Query, query), #(Candidate, candidate), False, renaming())
  |> result.map(fn(state) { Equivalent(cost: state.cost) })
  |> result.lazy_or(fn() {
    exact(#(Query, query), #(Candidate, candidate), True, renaming())
    |> result.map(fn(state) { Equivalent(cost: state.cost + permutation_cost) })
  })
  |> result.lazy_or(fn() {
    subsumed(query, candidate, False)
    |> result.map(fn(cost) { Subsumed(cost:) })
  })
  |> result.lazy_or(fn() {
    subsumed(query, candidate, True)
    |> result.map(fn(cost) { Subsumed(cost: cost + permutation_cost) })
  })
}

/// Unify a query type against a stored declaration. A `TypeDefinition` has no
/// single type to match against — its constructors are found by name, not by
/// shape — so it never verifies.
pub fn unify_declaration(
  query: Type,
  declaration: Declaration,
) -> Result(Match, Nil) {
  case declaration {
    ir.Function(signature:) -> unify(query, signature)
    ir.Constant(type_:) -> unify(query, type_)
    ir.TypeAlias(aliased:, ..) -> unify(query, aliased)
    ir.TypeDefinition(..) -> Error(Nil)
  }
}

/// The concrete structure a term carries — what a hole or a variable binding
/// absorbs when it matches it. Variables and holes weigh nothing (matching them
/// takes no liberty); every concrete node weighs 1.
fn weight(type_: Type) -> Int {
  case type_ {
    ir.Var(..) | ir.Hole -> 0
    ir.Named(args:, ..) -> 1 + weights(args)
    ir.Tuple(elements:) -> 1 + weights(elements)
    ir.Union(members:) -> 1 + weights(members)
    ir.Record(fields:) ->
      1 + list.fold(fields, 0, fn(acc, field) { acc + weight(field.type_) })
    ir.Fn(params:, return:, ..) ->
      1
      + list.fold(params, 0, fn(acc, param) { acc + weight(param.type_) })
      + weight(return)
  }
}

fn weights(types: List(Type)) -> Int {
  list.fold(types, 0, fn(acc, type_) { acc + weight(type_) })
}

// ─── Equivalent: alpha-equivalence via an injective variable mapping ─────────

/// Query variable index ↔ candidate variable index. Both directions are kept so
/// the mapping stays injective — two query variables can never collapse onto
/// one candidate variable, nor the reverse. `cost` accumulates what holes
/// absorbed and label surcharges.
type Renaming {
  Renaming(forward: Dict(Int, Int), backward: Dict(Int, Int), cost: Int)
}

fn renaming() -> Renaming {
  Renaming(forward: dict.new(), backward: dict.new(), cost: 0)
}

fn exact(
  query: #(Side, Type),
  candidate: #(Side, Type),
  permute: Bool,
  state: Renaming,
) -> Result(Renaming, Nil) {
  let #(_, query_type) = query
  let #(_, candidate_type) = candidate
  case query_type, candidate_type {
    // The wildcard: matches anything, binds nothing, costs what it absorbs.
    ir.Hole, _ ->
      Ok(Renaming(..state, cost: state.cost + weight(candidate_type)))
    // A query variable is not a wildcard. Only a candidate variable will do,
    // and only if the mapping stays injective.
    ir.Var(index: query_index, ..), ir.Var(index: candidate_index, ..) ->
      bind_injective(state, query_index, candidate_index)
    ir.Var(..), _ -> Error(Nil)
    _, ir.Var(..) -> Error(Nil)
    ir.Named(name: query_name, args: query_args),
      ir.Named(name: candidate_name, args: candidate_args)
    ->
      // The written name only. `package`/`module` serve linkability, and
      // `concept` is the tolerant tier's business.
      case query_name.name == candidate_name.name {
        True -> exact_list(query_args, candidate_args, permute, state)
        False -> Error(Nil)
      }
    ir.Tuple(query_elements), ir.Tuple(candidate_elements) ->
      exact_list(query_elements, candidate_elements, permute, state)
    // `is_method` is deliberately ignored: a receiver-first method and the
    // equivalent pipe-first function are the same shape, and that convergence
    // is the point of lowering methods receiver-first (see `signature-ir.md`).
    ir.Fn(params: query_params, return: query_return, is_method: _),
      ir.Fn(params: candidate_params, return: candidate_return, is_method: _)
    ->
      exact_params(query_params, candidate_params, permute, state, fn(state) {
        exact(
          #(Query, query_return),
          #(Candidate, candidate_return),
          permute,
          state,
        )
      })
    // `A | B` ≡ `B | A`: members match as a set, always.
    ir.Union(query_members), ir.Union(candidate_members) ->
      permuted(
        query_members,
        candidate_members,
        state,
        fn(q, c, state) { exact(#(Query, q), #(Candidate, c), permute, state) },
        Ok,
      )
    ir.Record(query_fields), ir.Record(candidate_fields) ->
      by_name(query_fields, candidate_fields, state, fn(q, c, state) {
        exact(#(Query, q.type_), #(Candidate, c.type_), permute, state)
      })
    _, _ -> Error(Nil)
  }
}

fn bind_injective(
  state: Renaming,
  query: Int,
  candidate: Int,
) -> Result(Renaming, Nil) {
  let Renaming(forward:, backward:, cost: _) = state
  case dict.get(forward, query), dict.get(backward, candidate) {
    Ok(bound), Ok(origin) if bound == candidate && origin == query -> Ok(state)
    Error(_), Error(_) ->
      Ok(
        Renaming(
          ..state,
          forward: dict.insert(forward, query, candidate),
          backward: dict.insert(backward, candidate, query),
        ),
      )
    _, _ -> Error(Nil)
  }
}

fn exact_list(
  queries: List(Type),
  candidates: List(Type),
  permute: Bool,
  state: Renaming,
) -> Result(Renaming, Nil) {
  positional(queries, candidates, state, fn(q, c, state) {
    exact(#(Query, q), #(Candidate, c), permute, state)
  })
}

fn exact_params(
  queries: List(Param),
  candidates: List(Param),
  permute: Bool,
  state: Renaming,
  then: fn(Renaming) -> Result(Renaming, Nil),
) -> Result(Renaming, Nil) {
  let unify = fn(q: Param, c: Param, state: Renaming) {
    use extra <- result.try(label_mismatch(q, c))
    exact(
      #(Query, q.type_),
      #(Candidate, c.type_),
      permute,
      Renaming(..state, cost: state.cost + extra),
    )
  }
  case permute && list.length(queries) <= permutation_limit {
    True -> permuted(queries, candidates, state, unify, then)
    False ->
      positional(queries, candidates, state, unify)
      |> result.try(then)
  }
}

// ─── Subsumed: one-sided matching (directional subsumption) ──────────────────
// `match(q, c, flexible, …)` asks whether the term on the `flexible` side is
// *at least as general as* the other: only the flexible side's variables bind,
// the rigid side is frozen. `flexible = Query` tests "query more general"
// (candidate is a specialization); `flexible = Candidate` tests the reverse.
// `Subsumed` takes the cheaper of the two directions. No occurs check is
// needed — a flexible variable only ever binds to a rigid subterm, which
// cannot contain it.

/// Flexible variable index → the frozen (other-side) term it is pinned to,
/// plus the cost of the structure absorbed along the way.
type Binding {
  Binding(substitution: Dict(Int, Type), cost: Int)
}

fn binding() -> Binding {
  Binding(substitution: dict.new(), cost: 0)
}

fn subsumed(query: Type, candidate: Type, permute: Bool) -> Result(Int, Nil) {
  let one_sided = fn(flexible) {
    match(query, candidate, flexible, permute, binding())
    |> result.map(fn(state: Binding) { state.cost })
  }
  case one_sided(Query), one_sided(Candidate) {
    Ok(a), Ok(b) -> Ok(int.min(a, b))
    Ok(a), Error(_) -> Ok(a)
    Error(_), Ok(b) -> Ok(b)
    Error(_), Error(_) -> Error(Nil)
  }
}

fn match(
  query: Type,
  candidate: Type,
  flexible: Side,
  permute: Bool,
  state: Binding,
) -> Result(Binding, Nil) {
  case query, flexible, candidate {
    // A query hole matches anything (holes only ever appear on the query
    // side), at the cost of what it absorbs.
    ir.Hole, _, _ -> Ok(Binding(..state, cost: state.cost + weight(candidate)))
    // The flexible side's variable binds to whatever the rigid side holds.
    ir.Var(index:, ..), Query, _ -> bind(state, index, candidate)
    _, Candidate, ir.Var(index:, ..) -> bind(state, index, query)
    _, _, _ -> match_structural(query, candidate, flexible, permute, state)
  }
}

/// Bind a flexible variable — charging the structure it absorbs — or check
/// consistency if already bound: a repeated variable must land on the *same*
/// rigid term each time, so `(a, a)` cannot match `(Int, Bool)`.
fn bind(state: Binding, variable: Int, frozen: Type) -> Result(Binding, Nil) {
  case dict.get(state.substitution, variable) {
    Ok(bound) ->
      case equal(bound, frozen) {
        True -> Ok(state)
        False -> Error(Nil)
      }
    Error(_) ->
      Ok(Binding(
        substitution: dict.insert(state.substitution, variable, frozen),
        cost: state.cost + weight(frozen),
      ))
  }
}

fn match_structural(
  query: Type,
  candidate: Type,
  flexible: Side,
  permute: Bool,
  state: Binding,
) -> Result(Binding, Nil) {
  case query, candidate {
    ir.Named(name: query_name, args: query_args),
      ir.Named(name: candidate_name, args: candidate_args)
    ->
      case query_name.name == candidate_name.name {
        True -> match_list(query_args, candidate_args, flexible, permute, state)
        False -> Error(Nil)
      }
    ir.Tuple(query_elements), ir.Tuple(candidate_elements) ->
      match_list(query_elements, candidate_elements, flexible, permute, state)
    ir.Fn(params: query_params, return: query_return, is_method: _),
      ir.Fn(params: candidate_params, return: candidate_return, is_method: _)
    ->
      match_params(
        query_params,
        candidate_params,
        flexible,
        permute,
        state,
        fn(state) {
          match(query_return, candidate_return, flexible, permute, state)
        },
      )
    ir.Union(query_members), ir.Union(candidate_members) ->
      permuted(
        query_members,
        candidate_members,
        state,
        fn(q, c, state) { match(q, c, flexible, permute, state) },
        Ok,
      )
    ir.Record(query_fields), ir.Record(candidate_fields) ->
      by_name(query_fields, candidate_fields, state, fn(q, c, state) {
        match(q.type_, c.type_, flexible, permute, state)
      })
    _, _ -> Error(Nil)
  }
}

fn match_list(
  queries: List(Type),
  candidates: List(Type),
  flexible: Side,
  permute: Bool,
  state: Binding,
) -> Result(Binding, Nil) {
  positional(queries, candidates, state, fn(q, c, state) {
    match(q, c, flexible, permute, state)
  })
}

fn match_params(
  queries: List(Param),
  candidates: List(Param),
  flexible: Side,
  permute: Bool,
  state: Binding,
  then: fn(Binding) -> Result(Binding, Nil),
) -> Result(Binding, Nil) {
  let unify = fn(q: Param, c: Param, state: Binding) {
    use extra <- result.try(label_mismatch(q, c))
    match(
      q.type_,
      c.type_,
      flexible,
      permute,
      Binding(..state, cost: state.cost + extra),
    )
  }
  case permute && list.length(queries) <= permutation_limit {
    True -> permuted(queries, candidates, state, unify, then)
    False ->
      positional(queries, candidates, state, unify)
      |> result.try(then)
  }
}

/// Structural equality of two frozen terms, used for the repeated-variable
/// consistency check. Variables compare by index (same namespace); unions and
/// records compare as sets, matching how the matcher treats them.
fn equal(a: Type, b: Type) -> Bool {
  case a, b {
    ir.Hole, ir.Hole -> True
    ir.Var(index: i, ..), ir.Var(index: j, ..) -> i == j
    ir.Named(name: n, args: na), ir.Named(name: m, args: ma) ->
      n.name == m.name && equal_list(na, ma)
    ir.Tuple(a), ir.Tuple(b) -> equal_list(a, b)
    ir.Fn(params: pa, return: ra, is_method: _),
      ir.Fn(params: pb, return: rb, is_method: _)
    -> equal_params(pa, pb) && equal(ra, rb)
    ir.Union(a), ir.Union(b) -> equal_set(a, b)
    ir.Record(a), ir.Record(b) -> equal_fields(a, b)
    _, _ -> False
  }
}

fn equal_list(a: List(Type), b: List(Type)) -> Bool {
  case a, b {
    [], [] -> True
    [x, ..a], [y, ..b] -> equal(x, y) && equal_list(a, b)
    _, _ -> False
  }
}

fn equal_params(a: List(Param), b: List(Param)) -> Bool {
  case a, b {
    [], [] -> True
    [x, ..a], [y, ..b] -> equal(x.type_, y.type_) && equal_params(a, b)
    _, _ -> False
  }
}

fn equal_set(a: List(Type), b: List(Type)) -> Bool {
  list.length(a) == list.length(b)
  && list.all(a, fn(x) { list.any(b, fn(y) { equal(x, y) }) })
}

fn equal_fields(a: List(Field), b: List(Field)) -> Bool {
  list.length(a) == list.length(b)
  && list.all(a, fn(x) {
    case list.find(b, fn(y) { y.name == x.name }) {
      Ok(y) -> equal(x.type_, y.type_)
      Error(_) -> False
    }
  })
}

// ─── Shared traversal helpers (generic over the matcher's state) ─────────────

/// Labels and spread markers are calling-convention detail, not shape. A label
/// the query wrote must match; a label the query omitted costs `label_cost`
/// when the candidate has one — the unlabelled candidate is the closer match.
fn label_mismatch(query: Param, candidate: Param) -> Result(Int, Nil) {
  case query.label, candidate.label {
    None, None -> Ok(0)
    None, Some(_) -> Ok(label_cost)
    Some(q), Some(c) ->
      case q == c {
        True -> Ok(0)
        False -> Error(Nil)
      }
    Some(_), None -> Error(Nil)
  }
}

fn positional(
  queries: List(a),
  candidates: List(a),
  state: s,
  unify: fn(a, a, s) -> Result(s, Nil),
) -> Result(s, Nil) {
  case queries, candidates {
    [], [] -> Ok(state)
    [query, ..queries], [candidate, ..candidates] -> {
      use state <- result.try(unify(query, candidate, state))
      positional(queries, candidates, state, unify)
    }
    _, _ -> Error(Nil)
  }
}

/// Field names are unique within a record, so a query field has at most one
/// candidate: no backtracking, and equal lengths make it a set equality. A
/// candidate with *extra* fields is structural subtyping — deferred.
fn by_name(
  queries: List(Field),
  candidates: List(Field),
  state: s,
  unify: fn(Field, Field, s) -> Result(s, Nil),
) -> Result(s, Nil) {
  case list.length(queries) == list.length(candidates) {
    False -> Error(Nil)
    True ->
      list.try_fold(queries, state, fn(state, query) {
        use candidate <- result.try(
          list.find(candidates, fn(candidate) { candidate.name == query.name }),
        )
        unify(query, candidate, state)
      })
  }
}

/// Match every query element against a distinct candidate, in any order,
/// backtracking on failure — *including* through `then`, the continuation run
/// once every element is paired: a binding made by one pairing can be what
/// makes a later pair fail, or what makes the continuation fail (a function's
/// return type, checked after its permuted parameters), so the first
/// successful pairing is not necessarily the one that completes.
fn permuted(
  queries: List(a),
  candidates: List(a),
  state: s,
  unify: fn(a, a, s) -> Result(s, Nil),
  then: fn(s) -> Result(s, Nil),
) -> Result(s, Nil) {
  case queries, candidates {
    [], [] -> then(state)
    [], _ | _, [] -> Error(Nil)
    [query, ..queries], _ ->
      list.fold_until(selections(candidates), Error(Nil), fn(_, selection) {
        let #(candidate, rest) = selection
        let matched = {
          use state <- result.try(unify(query, candidate, state))
          permuted(queries, rest, state, unify, then)
        }
        case matched {
          Ok(state) -> list.Stop(Ok(state))
          Error(_) -> list.Continue(Error(Nil))
        }
      })
  }
}

/// Every way to take one element out of a list: `[a, b]` → `[#(a, [b]), #(b, [a])]`.
fn selections(elements: List(a)) -> List(#(a, List(a))) {
  do_selections(elements, [])
}

fn do_selections(rest: List(a), seen: List(a)) -> List(#(a, List(a))) {
  case rest {
    [] -> []
    [element, ..others] -> [
      #(element, list.append(list.reverse(seen), others)),
      ..do_selections(others, [element, ..seen])
    ]
  }
}

fn succeeded(result: Result(a, Nil)) -> Bool {
  case result {
    Ok(_) -> True
    Error(_) -> False
  }
}

// ─── LEVEL 3 — ISOLATED. Not wired into the cascade. ─────────────────────────
// Full symmetric first-order unification: both sides bind at once. It accepts a
// strict superset of `Subsumed` — including pairs that only share a common
// instance without either being more general, e.g. `#(a, Int)` and `#(Bool, b)`
// (common instance `#(Bool, Int)`), which neither function can supply to the
// other. The cascade stops at directional subsumption because `_` already lets
// a user express looseness precisely, so this blunt tier is never load-bearing.
// Kept here, runnable and tested (and cost-free — it returns a plain Bool), so
// resurrecting it is adding a `Match` constructor that dispatches to it — not
// rewriting an algorithm.

type Variable =
  #(Side, Int)

type Term =
  #(Side, Type)

pub fn unify_symmetric(
  query: Type,
  candidate: Type,
  permute_params: Bool,
) -> Bool {
  succeeded(symmetric(
    #(Query, query),
    #(Candidate, candidate),
    permute_params,
    dict.new(),
  ))
}

fn symmetric(
  query: Term,
  candidate: Term,
  permute: Bool,
  substitution: Dict(Variable, Term),
) -> Result(Dict(Variable, Term), Nil) {
  let #(query_side, query_type) = resolve(query, substitution)
  let #(candidate_side, candidate_type) = resolve(candidate, substitution)
  case query_type, candidate_type {
    ir.Hole, _ -> Ok(substitution)
    ir.Var(index:, ..), _ ->
      bind_symmetric(
        #(query_side, index),
        #(candidate_side, candidate_type),
        substitution,
      )
    _, ir.Var(index:, ..) ->
      bind_symmetric(
        #(candidate_side, index),
        #(query_side, query_type),
        substitution,
      )
    ir.Named(name: query_name, args: query_args),
      ir.Named(name: candidate_name, args: candidate_args)
    ->
      case query_name.name == candidate_name.name {
        True ->
          symmetric_list(
            query_side,
            query_args,
            candidate_side,
            candidate_args,
            permute,
            substitution,
          )
        False -> Error(Nil)
      }
    ir.Tuple(query_elements), ir.Tuple(candidate_elements) ->
      symmetric_list(
        query_side,
        query_elements,
        candidate_side,
        candidate_elements,
        permute,
        substitution,
      )
    ir.Fn(params: query_params, return: query_return, is_method: _),
      ir.Fn(params: candidate_params, return: candidate_return, is_method: _)
    -> {
      let unify = fn(q: Param, c: Param, subst) {
        use _ <- result.try(label_mismatch(q, c))
        symmetric(
          #(query_side, q.type_),
          #(candidate_side, c.type_),
          permute,
          subst,
        )
      }
      let then = fn(subst) {
        symmetric(
          #(query_side, query_return),
          #(candidate_side, candidate_return),
          permute,
          subst,
        )
      }
      case permute && list.length(query_params) <= permutation_limit {
        True ->
          permuted(query_params, candidate_params, substitution, unify, then)
        False ->
          positional(query_params, candidate_params, substitution, unify)
          |> result.try(then)
      }
    }
    ir.Union(query_members), ir.Union(candidate_members) ->
      permuted(
        query_members,
        candidate_members,
        substitution,
        fn(q, c, subst) {
          symmetric(#(query_side, q), #(candidate_side, c), permute, subst)
        },
        Ok,
      )
    ir.Record(query_fields), ir.Record(candidate_fields) ->
      by_name(query_fields, candidate_fields, substitution, fn(q, c, subst) {
        symmetric(
          #(query_side, q.type_),
          #(candidate_side, c.type_),
          permute,
          subst,
        )
      })
    _, _ -> Error(Nil)
  }
}

fn symmetric_list(
  query_side: Side,
  queries: List(Type),
  candidate_side: Side,
  candidates: List(Type),
  permute: Bool,
  substitution: Dict(Variable, Term),
) -> Result(Dict(Variable, Term), Nil) {
  positional(queries, candidates, substitution, fn(q, c, subst) {
    symmetric(#(query_side, q), #(candidate_side, c), permute, subst)
  })
}

/// Follow a variable through the substitution until it reaches an unbound
/// variable or a non-variable term.
fn resolve(term: Term, substitution: Dict(Variable, Term)) -> Term {
  let #(side, type_) = term
  case type_ {
    ir.Var(index:, ..) ->
      case dict.get(substitution, #(side, index)) {
        Ok(bound) -> resolve(bound, substitution)
        Error(_) -> term
      }
    _ -> term
  }
}

fn bind_symmetric(
  variable: Variable,
  term: Term,
  substitution: Dict(Variable, Term),
) -> Result(Dict(Variable, Term), Nil) {
  let #(side, type_) = term
  let same = case type_ {
    ir.Var(index:, ..) -> #(side, index) == variable
    _ -> False
  }
  case same {
    True -> Ok(substitution)
    False ->
      // The occurs check: without it `($0, List<$0>)` against `(List<$1>, $1)`
      // would build the infinite type `List<List<List<…>>>` and never return.
      case occurs(variable, term, substitution) {
        True -> Error(Nil)
        False -> Ok(dict.insert(substitution, variable, term))
      }
  }
}

fn occurs(
  variable: Variable,
  term: Term,
  substitution: Dict(Variable, Term),
) -> Bool {
  let #(side, type_) = resolve(term, substitution)
  let occurs_in = fn(type_) { occurs(variable, #(side, type_), substitution) }
  case type_ {
    ir.Var(index:, ..) -> #(side, index) == variable
    ir.Hole -> False
    ir.Named(args:, ..) -> list.any(args, occurs_in)
    ir.Tuple(elements:) -> list.any(elements, occurs_in)
    ir.Union(members:) -> list.any(members, occurs_in)
    ir.Record(fields:) -> list.any(fields, fn(field) { occurs_in(field.type_) })
    ir.Fn(params:, return:, ..) ->
      list.any(params, fn(param) { occurs_in(param.type_) })
      || occurs_in(return)
  }
}
