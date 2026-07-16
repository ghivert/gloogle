# Signature IR — design

Status: **design, pre-implementation.**

## Why

Gloogle indexes Gleam today, but the target is multi-language search (Gleam, JS/TS, Python, Java). The frontend should
let a user type a signature "however they want" and still get relevant hits. To make that possible we canonicalize both
the stored signatures and the query into a **shared intermediate representation (IR)** of types, then match on the IR —
never on the surface string of any one language.

The `package.language` / `package.registry` columns (migration `20260703100000`) are the first step; this IR is the
second.

## Layered model

Every design decision lands in exactly one of three layers. The guiding rule for the first layer is:

> **A node exists only if it adds irreducible expressive power. If it is merely sugar over a combination of existing
> nodes, it is a front-end normalization rule, not a node.**

| Layer                                    | Responsibility                                                           | Examples                                                                                                        |
| ---------------------------------------- | ------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------- |
| **1. Nodes (IR)**                        | irreducible structure; what `type_search` matches on                     | `Fn`, `Named`, `Var`, `Tuple`, `Record`, `Union`, `Hole`                                                        |
| **2. Lowering (front-end)**              | converge a _divergent surface_ for the _same_ concept, deterministically | receiver-first methods; nullability (`Option(a)` ≡ `t \| null` ≡ `Optional[a]`)                                 |
| **3. Matching (unifier, tolerant tier)** | best-effort, lower-precision equivalences                                | name synonyms (`Int`≈`int`); wrapper transparency (`Promise`/`Future`/`Option` unwrap); arg permutation; `Hole` |

Consequences of the rule, already decided:

- **`Optional` is not a node.** It is sugar: `Named("Option", [t])` in Gleam, or `Union([t, Named("Null", [])])`
  structurally. Cross-language convergence is a _lowering_ rule (front-ends emit the nominal `Option` form), not a node.
- **`Promise` is not a node** and needs no lowering rule either — it is a plain `Named("Promise", [t])` in every
  language. Searching `-> Promise<B>` works for free via structural matching. "Async-agnostic" search (a bare `-> B`
  also surfacing `-> Promise<B>`) is a _matching-tier_ wrapper-transparency rule, the same mechanism that would give
  `Option`-transparency. Cross-language naming (`Promise`≈`Future`≈`Awaitable`) is the concept table (layer 3).
- **`Record` and `Union` are kept** even though Gleam type expressions never produce them today: they add real power
  (anonymous object/interface shapes = classes; anonymous `A | B`) that no combination of other nodes can express. They
  stay inert until TS/Python/Java ingestion lights them up.

## Two decided invariants

1. **Type variables are alpha-renamed to indices.** `(a) -> b` and `(x) -> y` are the same value. The IR stores
   `Var(Int)` only — **the source name is dropped**. The name is needed transiently during lowering (a
   `Dict(String, Int)` to bind repeated occurrences) and discarded afterwards. `parse.gleam` already does this today via
   `replace_indexed` producing `Index("", value)`; the IR formalizes it as `Var(Int)`. For display, readable names are
   regenerated deterministically from indices (`$0`→`a`, …) or read from the preserved raw `signature_`.

2. **A method is a receiver-first function.** An OO method is a `Fn` whose first parameter is the receiver, with
   `is_method: True`. This converges Java/Python/JS methods with Gleam's pipe-first idiom (`foo |> bar(x)`), where the
   receiver is already the first argument. The flag is retained only so same-shape methods can be boosted/filtered.

## Node set (Gleam types)

```gleam
/// A searchable signature is a `Type` — usually an `Fn`, but a bare type is valid
/// (constants, or a type-only query such as `Result<_, E>`).
pub type Type {
  Fn(params: List(Param), return: Type, is_method: Bool)
  Named(name: Name, args: List(Type))
  Var(index: Int)
  Hole
  Tuple(elements: List(Type))
  Record(fields: List(Field))
  Union(members: List(Type))
}

pub type Param {
  Param(label: Option(String), is_spread: Bool, type_: Type)
}

pub type Field {
  Field(name: String, type_: Type)
}

/// `qualifier` — namespace segments, e.g. ["gleam", "option"] or ["java", "util"].
/// `base`      — the written type name, e.g. "Option", "List".
/// `concept`   — cross-language canonical id filled by the synonym table (layer 3),
///               e.g. base "Int" → concept "int". Exact match uses `base`; conceptual
///               match uses `concept` when present.
pub type Name {
  Name(qualifier: List(String), base: String, concept: Option(String))
}
```

## Canonical notation (BNF)

One parser + one printer handle this notation. It is the query-side syntax, the re-readable stored canonical string, and
unambiguous by construction (variables are `$n`, no lexical var-vs-nominal guessing). Each production is annotated with
the node it produces.

```bnf
signature  ::= type                                          ; an item is a Type (often an Fn)

fn         ::= [ recv "::" ] "(" [ params ] ")" "->" type    ⟶ Fn(params, ret, method)
recv       ::= type                                          ; present ⇒ method=True, receiver = 1st param
params     ::= param { "," param }
param      ::= [ label ":" ] [ "..." ] type                  ⟶ Param(label?, is_spread, type)
label      ::= lower-ident

type       ::= union
union      ::= atom { "|" atom }                             ⟶ Union([…])   ; single member ⇒ passthrough
atom       ::= named | var | hole | tuple | record | fn | "(" type ")"

named      ::= qname [ "<" args ">" ]                        ⟶ Named(name, args)
var        ::= "$" digit+                                    ⟶ Var(index)
hole       ::= "_"                                           ⟶ Hole
tuple      ::= "#(" [ args ] ")"                             ⟶ Tuple([…])
record     ::= "{" [ fields ] "}"                            ⟶ Record([…])
field      ::= field-name ":" type                           ⟶ Field(name, type)

args       ::= type { "," type }
fields     ::= field { "," field }
qname      ::= { segment "." } base                          ⟶ Name(qualifier, base, concept?)
base       ::= upper-ident
segment    ::= ident
field-name ::= ident
```

Notes:

- There is **no `?`** in the notation — nullability is resolved by the front-end (layer 2) before it reaches the
  canonical form.
- `fn` is both the top level and an `atom` (higher-order params like `($0) -> $1`); the same production is reused.
- Grouping `"(" type ")"`, tuple `"#(" … ")"`, and function `"(" params ")" "->"` are disambiguated by `#` and the
  trailing `->` (backtracking parser).

## Worked examples

### `result.map` — Gleam and TS canonicalize identically

- Gleam: `fn(Result(a, b), fn(a) -> c) -> Result(c, b)`
- TS: `function map<A, B, C>(r: Result<A, B>, f: (a: A) => C): Result<C, B>`

Both front-ends produce the same canonical form (the Gleam front-end knows `a,b,c` are variables by the lowercase
convention; the TS front-end knows `A,B,C` are variables from the `<A,B,C>` binder):

```
( Result<$0, $1>, ($0) -> $2 ) -> Result<$2, $1>
```

IR:

```
Fn(
  params: [
    Param(None, False, Named("Result", [Var(0), Var(1)])),
    Param(None, False, Fn([Param(None, False, Var(0))], Var(2), is_method: False)),
  ],
  return: Named("Result", [Var(2), Var(1)]),
  is_method: False,
)
```

### Method convergence

Java `class List<A> { A get(int i) }` lowers to receiver-first:

```
List<$0> :: (Int) -> $0        ≡   Fn([Param(_,_,Named "List" [$0]), Param(_,_,Named "Int" [])], Var(0), is_method: True)
```

The leading `(List<$0>, Int)` is shared with the equivalent Gleam pipe-first function, so OO methods and Gleam functions
unify.

### Async

`(A) -> Promise<B>` is `Fn([Param(_,_,Var 0)], Named("Promise",[Var 1]), is_method: False)` — `Promise` is just a
`Named`. Explicit async search works structurally; transparent async-agnostic search is a layer-3 unwrap rule.

## Type search: fingerprint + verify

The in-app type search (trie + actor + `cell`, in `jupiter/gleam/type_search*`) is being
**replaced** by a Postgres-native **fingerprint + verify** design (see the "Type search
algorithm review" work). No in-memory index, no actor, no boot-time scan.

- **Phase 1 — candidate selection (GIN).** Each function row carries a fingerprint: `arity int`
  and `type_names text[]` (the bag of concrete type names it mentions, nesting flattened;
  variables and holes contribute nothing). A GIN index on `type_names` answers
  `WHERE arity = $1 AND type_names @> ARRAY[...]` in milliseconds, narrowing 50k rows to a few
  hundred candidates. Fuzzy-name resolution happens here, once per distinct query name.
- **Phase 2 — verification.** Each candidate's IR is unified against the query IR (exact
  structure, `Var` binding injective, `Hole` matches anything, optional arg permutation). This
  consumes the **stored IR** directly.

Both phases derive from the IR, so the IR is what makes the query language-agnostic.

## Storage & re-derivability

`signature_` is the **source of truth** and is preserved; everything else is _derived_ and
re-generable by re-running ingestion (so the IR can evolve without lock-in). Per
`package_type_fun_signature` row:

```
signature_  (root, source of truth — the rendered target-language signature)
   ├─ target-language parse ─────→ signature_json     (display; per-language, faithful; renamed from json_signature)
   └─ language front-end → IR
          ├─ serialize ──────────→ signature_ir_json  (jsonb; phase-2 verify input: parse-free, language-agnostic)
          └─ fingerprint ────────→ arity + type_names (phase-1 GIN)

signature_compact  (derived from signature_, unchanged: content_search lexical/trigram tier)
```

Notes:

- `signature_ir_json` is the **materialized phase-2 input** — the alpha-renamed canonical IR as
  jsonb. Decoding jsonb is cheaper and more robust than re-parsing a grammar string, and keeps
  the verifier language-agnostic (it never re-parses the Gleam-specific `signature_`).
- `signature_json` (faithful, target-language) and `signature_ir_json` (canonical IR) are **not**
  redundant: different data, different purpose (display vs matching).
- The canonical IR **string** (`ir.to_string`) is **not stored** — it lost variable names to
  alpha-renaming so it cannot serve the lexical tier (that stays on `signature_compact`), and it
  is regenerable on demand via `ir.to_string(decode(signature_ir_json))`. Add a debug column later
  only if wanted.

## Scope

- **Now:** freeze the node set above; build the IR types (done) + canonical printer (done); the Gleam front-end
  (`pi.Type → ir.Type` lowering, _not_ a text parser — the structured `package_interface` is already available at
  ingestion); the fingerprint (`arity`, `type_names`) + verify unifier; and the query-side canonical parser
  (`String → Type`). Remove the old trie/actor `type_search`.
- **Later, per language:** TS/Python/Java front-ends; the concept/synonym table; the tolerant matching rules (wrapper
  transparency, subtyping).
- **Out of scope v1 (extension points):** generic bounds (`<T extends Comparable>`), default parameter values,
  effect/async annotations beyond nominal wrappers, and subtyping (see below). Tracked in `signature-ir-todos.md`.

### Subtyping / inheritance (deferred)

Inheritance is a **relation between nominal types**, not part of a single type expression, so it is never an IR node.
When added it splits across two existing layers, and comes in two flavours:

- **Nominal subtyping** (declared `extends`/`implements`, e.g. Java, Gleam-ish): needs an external **subtype-edge table**
  populated at ingestion (`Dog <: Animal`, `ArrayList <: List`) — the sibling of the concept table — plus a tolerant
  unifier rule that walks those edges (query `Animal` matches candidate `Dog`, lower precision than exact).
- **Structural subtyping** (TS object shapes: `{name, age, email}` is-a `{name, age}`): **no table** — just a matching
  rule "the candidate `Record` contains *at least* the query's fields" (field superset), derivable straight from the nodes.

Both live in the tolerant matching tier; the nodes are unchanged. In v1, a query `Animal` will not surface `Dog`, and
a `Record` query must match exactly.

## Open questions

- Concept table location: a static table in code vs a DB table joined at ingestion.
- Whether the fingerprint's `type_names` should store `Name.base` or the resolved `concept` (synonym expansion at
  ingestion vs at query time).
