# Signature IR — deferred work / TODO

Companion to `signature-ir.md`. Everything here is **deliberately out of the v1 IR** and
recorded so it is not forgotten. The guiding invariant: none of these changes the node
set — they are front-end lowering rules, matching-tier rules, or external data tables.

## Build order (v1)

- [x] `data/ir` types (+ `Name.package`) + `to_string` printer + tests — **done**
- [x] Gleam front-end — `pi.Type → ir.Type` converter (`data/ir/from_gleam`; pure, no
      ctx/DB; `Var` keeps pi ids for now). One `ir/from_<language>` per source language — **done**
- [ ] Shared alpha-renaming pass (post-lowering: `Var` pi ids → first-appearance 0,1,2…)
- [ ] Storage columns — `signature_ir_json` (jsonb), `arity`, `type_names` (see Storage below);
      rename `json_signature` → `signature_json`
- [ ] Fingerprint — compute `arity` + `type_names` (bag of concrete names) from IR; GIN index
- [ ] Verify unifier — unify query IR vs candidate IR (`Var` injective, `Hole` any, permutations)
- [ ] `data/ir/parse` — query-side canonical parser (`String → Type`) for user-typed queries
- [ ] Remove old trie/actor type search (`jupiter/gleam/type_search*`, `search`, `cell` usage,
      `ctx.search`, worker registration); it is superseded by fingerprint + verify

## Deferred — matching tier (tolerant)

- [ ] **Name synonyms / concept table.** `Int`≈`int`≈`Integer`, `String`≈`str`≈`string`.
      Fills `Name.concept`. One-to-many cases (`number`→{Int,Float}) stay here, never in the
      exact form. Location undecided: static table in code vs DB table joined at ingestion.
- [ ] **Wrapper transparency.** Optional async/effect-agnostic search: a bare `-> B` also
      surfaces `-> Promise<B>` / `-> Future<B>` / `-> Option(B)` by unwrapping known wrappers.
      Same mechanism gives `Option`-transparency. Lower precision than exact.
- [ ] **Nominal subtyping.** Declared `extends`/`implements`. Needs an external **subtype-edge
      table** (`Dog <: Animal`) populated at ingestion + a unifier rule that walks the edges.
- [ ] **Structural subtyping.** TS object shapes: a candidate `Record` matches a query `Record`
      when it contains *at least* the query's fields (field superset). No table needed.

## Deferred — front-end lowering (per language, deterministic)

- [ ] **Nullability convergence.** `Option(a)` (Gleam) ≡ `t | null` / `t?` (TS) ≡ `Optional[a]`
      (Python) — front-ends emit the nominal `Option` form. No `Optional` node.
- [ ] **Receiver-first methods.** OO methods lower to `Fn` with `is_method: True`, receiver as
      first param. (Design decided; applies when TS/Java/Python front-ends land.)
- [ ] **Classes.** A class decomposes at ingestion into: nominal head `Named`, a `Record` of its
      fields (method members = `Field` whose `type_` is an `Fn`), and receiver-first `Fn` method
      entries. Needs a new `kind` value ("class"). No new node.

## Deferred — IR node extensions (only if a language forces it)

- [ ] **Generic bounds.** `<T extends Comparable>` — would attach an optional bound to a variable
      binder. Out until a target language makes it load-bearing for search.
- [ ] **Default parameter values.** Ingestion/display detail; likely never a matching concern.

## Cross-reference (`ref`) resolution — moved to query time

Decision: **stop storing the resolved `ref`** (in `signature_json` or anywhere). A `ref` resolves a
semver *range* against an *evolving* set of published versions, so freezing it at ingestion is stale
by construction. Resolve on demand instead, against current DB state, respecting the requirement —
this also matches what the user resolves locally, and deletes the stateful ingestion machinery.

- [ ] Drop `ref` computation from ingestion (`generate/types` `extract_parameters_relation`,
      `find_type_signature`, the cross-package "restart extraction" logic, the `ctx`/`Loss` threading).
- [ ] `Name` carries `package` + `qualifier` + `base` (done) → the nominal coordinates needed to resolve.
- [ ] **`package_dependency` normalized table** (preferred over jsonb: cleaner joins, data analysis):
      `(id, package_release_id, name, requirement, kind ['dependency'|'dev-dependency'])`,
      `unique (package_release_id, name, kind)`, index on `name`. Populated at ingestion from the
      already-stored `package_release.gleam_toml`; backfill existing rows.
- [ ] Query-time resolution for displayed rows: row → release → `package_dependency` (constraint) →
      `keep_matching_releases`/`semver.is_match` (dynamic, existing logic) → target release/signature.
      Reuse `toml.find_package_requirement` logic, debranched from `ctx`. Self-package type → the
      release's own version.
- [ ] NULL `gleam_toml` (nullable column) or dependency not yet indexed → **unresolved link** (show
      the type name un-linked / hex link).

## Storage / integration

Column layout on `package_type_fun_signature`, all derived from `signature_` (source of truth):

- `signature_` — source string (display + FTS). **Unchanged.**
- `signature_json` — target-language faithful parse, for display. **Rename** of `json_signature`
  (follow the rename through `SELECT` aliases, `type_search.decoder()`, frontend).
- `signature_ir_json` — **new**, jsonb; the alpha-renamed canonical IR; phase-2 verify input.
- `arity int` + `type_names text[]` — **new**; the fingerprint; GIN index on `type_names`.
- `signature_compact` — **unchanged**; `content_search` lexical/trigram tier (NOT replaced by IR —
  the canonical form loses variable names, so it can't serve full-text).

- [ ] The canonical IR **string** is deliberately **not stored** (derivable via
      `ir.to_string(decode(signature_ir_json))`); add a debug column later only if wanted.
- [ ] Backfill existing rows (keyset pagination): lower `signature_`/`pi` → IR → fill the new columns.
- [ ] Multi-language: populate `package.language` / `package.registry` (columns already added,
      migration `20260703100000`) as non-Gleam ingestion lands.
