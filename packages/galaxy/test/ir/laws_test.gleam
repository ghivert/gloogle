//// Property tests — the laws of the IR, proven on random types instead of
//// asserted on hand-picked examples. Generators in `ir/generators`.

import data/ir
import data/ir/canonical
import data/ir/unify
import gleam/json
import gleam/option.{None}
import gleam/result
import ir/generators as gen
import qcheck

const runs = 300

fn config() {
  qcheck.default_config() |> qcheck.with_test_count(runs)
}

fn check(generator: qcheck.Generator(a), property: fn(a) -> Nil) -> Nil {
  qcheck.run(config(), generator, property)
}

// ─── Codec ───────────────────────────────────────────────────────────────────

// `decoder` reads back exactly what `encode` wrote, for every declaration.
pub fn codec_round_trip_test() {
  use declaration <- check(gen.declaration())
  let json = json.to_string(ir.encode(declaration))
  assert json.parse(json, ir.decoder()) == Ok(declaration)
}

// ─── Alpha-renaming ──────────────────────────────────────────────────────────

// Renaming an already-renamed type changes nothing.
pub fn alpha_rename_idempotent_test() {
  use type_ <- check(gen.type_(gen.any))
  let once = ir.alpha_rename(type_)
  assert ir.alpha_rename(once) == once
}

// The same, through a whole declaration (constructors share one numbering).
pub fn alpha_rename_declaration_idempotent_test() {
  use declaration <- check(gen.declaration())
  let once = ir.alpha_rename_declaration(declaration)
  assert ir.alpha_rename_declaration(once) == once
}

// ─── Printer / parser ────────────────────────────────────────────────────────

// The canonical notation loses nothing but the source names of variables:
// what `to_string` prints, `canonical.parse` reads back, holes, labels, spreads,
// methods, qualified names included.
pub fn print_parse_round_trip_test() {
  use type_ <- check(gen.type_(gen.any))
  let printed = ir.to_string(type_)
  assert canonical.parse(printed) == Ok(gen.nameless(type_))
}

// The normal form is a fixed point: parsed back and normalised again, it is
// the same string. (Record fields and union members are already sorted, labels
// and qualification already gone.)
pub fn canonical_fixed_point_test() {
  use type_ <- check(gen.type_(gen.any))
  let normal = ir.to_canonical(type_)
  let again =
    canonical.parse(normal)
    |> result.map(ir.to_canonical)
  assert again == Ok(normal)
}

// Found by the law above: two union members equal once unqualified are one
// member of the normal form, not two that the parser would then collapse.
pub fn canonical_dedupes_unqualified_union_members_test() {
  let list_of_list = fn(module) {
    ir.Named(ir.Name("", module, "List", None), [
      ir.Named(ir.Name("", "", "List", None), []),
    ])
  }
  let type_ =
    ir.Named(ir.Name("", "gleam", "Result", None), [
      ir.Union([list_of_list("gleam"), list_of_list("")]),
    ])
  let normal = ir.to_canonical(type_)
  assert normal == "Result<List<List>>"
  assert canonical.parse(normal) |> result.map(ir.to_canonical) == Ok(normal)
}

// The normal form forgets exactly what the exact tier ignores: labels, the
// method flag, the module qualification. Two types that differ only there have
// one normal form.
pub fn canonical_ignores_labels_and_qualification_test() {
  use type_ <- check(gen.type_(gen.any))
  let stripped =
    gen.map_type(type_, fn(node) {
      case node {
        ir.Fn(params, return, _) ->
          ir.Fn(
            gen.map_params(params, fn(param) { ir.Param(..param, label: None) }),
            return,
            False,
          )
        ir.Named(name, args) -> ir.Named(ir.Name(..name, module: ""), args)
        other -> other
      }
    })
  assert ir.to_canonical(stripped) == ir.to_canonical(type_)
}

// ─── Matcher ─────────────────────────────────────────────────────────────────

// Reflexivity: a stored type matches itself exactly, at cost 0 — whatever its
// labels, its method flag, its variables.
pub fn unify_reflexive_test() {
  use type_ <- check(gen.type_(gen.stored))
  assert unify.unify(type_, type_) == Ok(unify.Equivalent(0))
}

// Alpha-invariance: renaming the variables of either side changes nothing.
pub fn unify_alpha_invariant_test() {
  use type_ <- check(gen.type_(gen.stored))
  let renamed = ir.alpha_rename(type_)
  assert unify.unify(type_, renamed) == Ok(unify.Equivalent(0))
  assert unify.unify(renamed, type_) == Ok(unify.Equivalent(0))
}

// A hole absorbs anything: punch one anywhere in a stored type and the result,
// as a query, still matches the type exactly — and the cost is positive
// exactly when what the hole replaced was concrete structure.
pub fn hole_absorbs_test() {
  let typed =
    qcheck.bind(gen.type_(gen.stored), fn(type_) {
      qcheck.map(gen.position(type_), fn(at) { #(type_, at) })
    })
  use #(type_, at) <- check(typed)
  let query = gen.replace(type_, at, ir.Hole)
  let assert Ok(replaced) = gen.node_at(type_, at)
  let replaced_a_variable = case replaced {
    ir.Var(..) -> True
    _ -> False
  }
  let assert Ok(unify.Equivalent(cost)) = unify.unify(query, type_)
  assert { cost == 0 } == replaced_a_variable
}

// Generalising a candidate keeps it matching: replace any node of a stored
// type by a fresh variable and the original, as a query, still finds it — as
// an equivalent if the node was a once-occurring variable, subsumed otherwise.
pub fn generalised_candidate_matches_test() {
  let typed =
    qcheck.bind(gen.type_(gen.closed), fn(type_) {
      qcheck.map(gen.position(type_), fn(at) { #(type_, at) })
    })
  use #(type_, at) <- check(typed)
  let candidate = gen.replace(type_, at, ir.Var(99, "fresh"))
  assert result.is_ok(unify.unify(type_, candidate))
}

// `compare` is a total order with `Equivalent` above `Subsumed`.
pub fn compare_buckets_test() {
  use #(a, b) <- check(qcheck.tuple2(
    qcheck.bounded_int(0, 9),
    qcheck.bounded_int(0, 9),
  ))
  assert unify.compare(unify.Equivalent(a), unify.Subsumed(b)) == order_lt()
  assert unify.compare(unify.Subsumed(a), unify.Equivalent(b)) == order_gt()
}

fn order_lt() {
  unify.compare(unify.Equivalent(0), unify.Equivalent(1))
}

fn order_gt() {
  unify.compare(unify.Equivalent(1), unify.Equivalent(0))
}
