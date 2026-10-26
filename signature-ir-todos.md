# Signature IR — deferred work / TODO

Companion to `signature-ir.md`. Everything here is **deliberately out of the v1 IR** and
recorded so it is not forgotten. The guiding invariant: none of these changes the node
set — they are front-end lowering rules, matching-tier rules, or external data tables.

## Build order (v1)

- [x] `data/ir` types (+ `Name.package`) + `to_string` printer + tests — **done**
- [x] Gleam front-end — `pi.Type → ir.Type` converter (`data/ir/from_gleam`; pure, no
      ctx/DB; `Var` keeps pi ids for now). One `ir/from_<language>` per source language — **done**
- [x] Shared alpha-renaming pass (`ir.alpha_rename`: `Var` pi ids → first-appearance 0,1,2…;
      reading order = params then return; preserves `Var.name`) — **done**
- [x] `Var(index, name)` — index (matching, precomputed) + name (display); `from_gleam` fills the
      name from the id letter — **done**
- **Single-store consolidation** (design: design doc "Single stored representation"):
  - [x] `ir.Declaration` (`Function`/`Constant`/`TypeAlias`/`TypeDefinition`) + `ir.Constructor` —
        structural only, metadata stays in columns.
  - [x] `from_gleam` covers all 4 kinds (`pi.* → ir.Declaration`) + `pi.Type → ir.Type`.
  - [x] `data/ir` `encode(Declaration) → Json` + `decoder()`, self-describing, round-trip tested.
  - [x] Wire ingestion: `from_gleam` → `alpha_rename_declaration` → `ir.encode` → store in the
        `signature_ir` jsonb column (migration `20260703100003`). All 4 `api/signatures` call sites
        populate it; upfront recursive-type placeholders write jsonb `null`. — **done**
  - [ ] Retire `generate/types` + the `signature_json` column once display reads the IR (gated on
        the venus refactor below).
  - [x] Move `data/ir` into `galaxy` (module path `data/ir` unchanged, so no import churn; no new
        galaxy dependency — `decrypt` already comes via `plasma`). The Gleam front-end
        `data/ir/from_gleam` **stays in jupiter**: it is the only piece needing
        `gleam_package_interface`, so keeping it out of galaxy is what lets venus drop `pi`.
        IR unit tests moved to `packages/galaxy/test/ir_test.gleam`; galaxy's test runner fixed
        (`interfaces_test.gleam` → `galaxy_test.gleam`, it was missing so galaxy tests never ran). — **done**
  - [ ] Refactor venus to render the IR (drops its `pi` dependency, computes `width` at render).
        `signature_` stays as ground truth. Gates retiring `signature_json`.
  - [x] Fingerprint `arity` + `type_names` derived from the IR (`ir.fingerprint`); columns +
        GIN index on `type_names` (migration `20260703100004`); populated at every `api/signatures`
        call site. v1 stores bare `Name.name` (concept expansion deferred). — **done**
- [x] Verify unifier — unify query IR vs candidate IR (`Var` injective via `index`, `Hole` any,
      permutations). `data/ir/unify` in galaxy (`unify` on `Type`, `unify_declaration` on the stored
      `Declaration`; `TypeDefinition` never verifies — its constructors are found by name, not shape).
      Exact tier only. Decisions: `Var` matches a candidate `Var` **only** (a query var is not a
      wildcard — `($0) -> $0` asks for the identity, not for `Int -> Int`), under a bidirectionally
      injective mapping; `Hole` is the wildcard; `is_method` is ignored (that convergence is the point
      of receiver-first lowering); `Named` compares the written `name` alone (`package`/`module` serve
      linkability, `concept` is the tolerant tier); a query label constrains, an omitted one does not;
      spread markers never constrain; `Union` members always match as a set, `Record` fields by name
      and exactly (field-superset = structural subtyping, deferred); `Tuple` stays positional.
      `Options(permute_params:)` is off by default and falls back to positional beyond 6 params
      (factorial search). **Now wired into the cascade** — `handlers.exec_type_search` ranks the
      verified candidates with the new `unify.compare` (`Equivalent` above `Subsumed`, cost within). — **done**
- [x] **Two tiers.** `Options(tier:)` — `Exact` first, then `Lenient`, the latter a strict superset
      (so a second pass only ever adds). `Lenient` = **directional subsumption**: one side is more
      general than the other. `($0) -> $0` finds `Int -> Int` (query more general) *and* `(List<Int>)
      -> Bool` finds `fn(List(a)) -> Bool` (candidate more general). Implemented as one-sided matching
      run both ways (query-flexible, then candidate-flexible); no occurs check needed (a flexible var
      only binds to a frozen subterm). `Hole` is the wildcard in both tiers; a once-occurring `Var`
      behaves like a `Hole` in `Lenient` — the var/hole difference bites only on repeated variables
      and in `Exact`. — **done**
- [x] **Stop at level 2, not full unification.** `Lenient` deliberately rejects pairs that only share
      a common instance without either being more general — e.g. `#(a, Int)` vs `#(Bool, b)` (common
      instance `#(Bool, Int)`, but neither can serve the other). Rationale: `_` lets the user ask for
      looseness *precisely*, so the blunt common-instance match is never the only route to a wanted
      result. Note the correction: `(a, b) -> a` vs `(a, a) -> a` is **not** a common-instance-only
      case — `(a, b) -> a` is strictly more general (substitute `b := a`), so it *is* comparable and
      `Lenient` accepts it. Full symmetric unification is kept, tested, and isolated in
      `unify_symmetric` (both sides bind, with the occurs check); resurrecting it = adding a `Tier`
      value that dispatches to it, not rewriting an algorithm. — **done**
- [x] **Cost verdict (`Match`) — supersedes the `Options(tier:, permute_params:)` API of the two
      entries above.** `unify`/`unify_declaration` return `Result(Match, Nil)` with
      `Equivalent(cost)` / `Subsumed(cost)`: constructors are the result buckets (equivalence above
      subsumption), `cost` ranks inside a bucket — the Hoogle lesson (matching returns a cost, the
      cost is the ranking) made native. Weights: 1 per concrete node absorbed by a `Hole` or a
      variable binding (`weight`; variables and holes weigh 0, so the genuinely polymorphic
      candidate ranks first), `label_cost` for a candidate label the query omitted,
      `permutation_cost` when arguments had to be reordered. Permutation is no longer an option but
      a **priced rewrite**: positional first, reordered second, still skipped beyond 6 params — and
      the permutation search now backtracks *through the return type* (a pairing whose bindings
      doom the return is undone; the old search stopped at the parameters). Subsumption keeps the
      cheaper of its two directions. First-match cost, not a proven minimum. `unify_symmetric`
      stays an isolated Bool. — **done**
- [ ] **Property-based tests (qcheck) on the matcher.** The laws are asserted on single examples
      today; generators over random IR would prove them universally: `unify(t, t) ==
      Ok(Equivalent(0))` for hole-free, label-free `t`; `Equivalent(_)` ⟹ the subsumption pass
      also accepts (the superset law that makes the second bucket only ever add);
      `alpha_rename` idempotent; `encode`/`decoder` round-trip on random declarations;
      `to_string`/`parse` round-trip once `data/ir/parse` exists.
- [x] **Phase 1 SQL — core, rescue and first measurements landed.** Implemented as
      `queries.fingerprint_candidates` and wired through `handlers.exec_type_search` (parse →
      phase 1 → phase 2 `unify_declaration` → sort by `unify.compare` → take 20). What is **live**:
      `WHERE arity = $1 AND type_names @> $2 AND signature_ir IS NOT NULL ORDER BY
      cardinality(type_names), p.rank DESC LIMIT 100`, verify in Gleam, truncate to 20. Used
      computed `cardinality()` (no stored count column yet) and `@>`-only (no rescue yet). What
      **remains**: the candidate-more-general rescue (`OR has_variables` vs `<@`, below), the
      stored-`type_names_count`-vs-`cardinality()` decision. **First measurement (2026-10-26, local
      snapshot backfilled, 502k rows, Postgres 18, `ANALYZE` run):** the GIN is used for every query
      (Bitmap Index Scan on `type_names`), rare names answer in 5–18 ms (`{Request,Response}` 1,597
      index hits → 4.8 ms; `{Decoder}` 14,725 hits → 18 ms), the frequent ones in 90–130 ms (`{Int}`
      63,833 hits → 89 ms; `{List}` 113,028 hits → 130 ms, parallel). In every plan the `arity = $1`
      filter is applied *after* the index and removes 60–80 % of the heap rows fetched. **Composite
      index landed the same day** (`20261026130000`, `btree_gin`, `gin (arity, type_names)`, replaces the
      single-column GIN): real cascade query, warm cache, `{List}` 128 → 44 ms, `{Int}` 120 → 22 ms,
      `{Decoder}` 14 → 2.5 ms, `{Request,Response}` 3 → 2 ms; no heap filter left but
      `signature_ir IS NOT NULL`; 5 MB, < 2 s to build. Still open: `type_names_count`.
      **Rescue landed too** (`20261026140000`): the candidate-more-general direction is
      `type_names_key = ANY(proper subsets of the query's names)` — `type_names` is now stored sorted
      (canonical, `ir.names` sorts), `type_names_key` is the `,`-joined key (a *text* column because the
      GIN claims `=` on the array, with its recheck), btree on `(arity, type_names_key)`,
      `queries.subset_keys` enumerates 2^n − 1 keys for n ≤ 6; ordering became
      `abs(cardinality(type_names) − n)`. Measured and rejected: `<@` (75–320 ms, GIN recheck of every
      row sharing a name), array equality (700 ms, GIN again). Cost: real cascade query, warm cache, on
      `(List(Int)) -> Bool` 12–16 ms without → 38–52 ms with (≈2,300 generic rows fetched, ranked last);
      on `{List}` (32k rows read) ~45 ms either way. `OR has_variables` is superseded. Still missing: a
      more general candidate whose names sit only under a query hole (`fn(List(a)) -> b` for
      `(_) -> Bool`) — sound selection there is `arity` alone; deferred. Note for prod: both migrations
      mass-update the table, so bloat follows (1.6 GB → 809 MB after `VACUUM FULL` locally); let
      autovacuum reclaim it or schedule one. **Dedupe**: phase 1 returns one row per declaration
      (`DISTINCT ON (package, module, name, kind)`, latest semver), as the old tier and the content tiers
      do; measured cost on `{List}` (32k candidate rows): ~45 ms → 240–300 ms, of which ~80 ms is the
      per-row semver regexp (`string_to_array(regexp_replace(version…))`) and the rest the joins over
      32k rows; negligible on 2–3-name queries. **`version_ordering` landed** (`20261026150000`): a
      generated stored column on `package_release`, so all six tiers read it instead of the per-row
      regexp (`{List}` 910 → 710 ms, `fn(List(Int)) -> Bool` 190 → 120 ms, `name_search` 63 → 48 ms on
      this machine). What remains on frequent names is the heap scan of the 33k candidate rows
      (~260 ms here): the follow-up, not taken, is restricting phase 1 to each package's latest
      release with signatures (~5k rows, no `DISTINCT ON` needed; drops declarations removed from the
      latest version — arguably what a search of the current ecosystem wants). **Placeholders**:
      123 rows (24 packages, 37 releases) were recursive-type placeholders whose second upsert never
      landed (`signature_ = ''`, JSON `null` in `signature_json`/`metadata`); the backfill gave them an IR,
      the empty subset key made them phase-1 candidates for arity-0 queries, and one undecodable row
      fails `type_search.decoder()` for the whole batch — a silent empty tier (the name tiers were
      already exposed in prod: "Connection", "Error"). Fixed twice over: every tier filters
      `signature_ <> ''`, and `tasks/reextract` replays the ingestion chain from the stored
      `package_interface`/`gleam_toml` for the affected releases (37/37 ok locally, 0 left; the
      original failure does not reproduce with current code — run it in prod after deploy). Also
      fixed on the way: `generate/types` wrote the *documentation* into every `signature_json.deprecation`
      (copy-paste); it now writes the deprecation message. **Shape guards** (found by timing the
      end-to-end run, 1–16 s per request before): `kind <> 'type_definition'` in phase 1 (phase 2 never
      verifies one, and the 22,771 parameterless enums all carry the empty key: `List(Int)` 1.4 s →
      0.4 s), and a nameless query (`fn(a) -> a`) skips `@>` (`@> '{}'` is true for the whole arity,
      3 s) for the empty key alone (112 ms). The text tiers have their own, older costs
      (`signature_search`/`documentation_search` ~140 ms each on "Error" with their tsvector GINs,
      the trigram `content_search` worse on type-shaped strings) — not touched. Local stack: the
      compose now gives Postgres 384 MB of `shared_buffers`; with the 128 MB default, a 32k-row
      candidate set re-read 150 MB of pages per query and timings swung 10x (1 GB was tried and pushed
      the laptop into swap). **End to end** (jupiter on the local stack, Hex cursor parked so the sync
      stays idle, 2026-10-26): the type tier answers on every shape — `list.map` for the map query,
      `list.is_empty` for `(List(Int)) -> Bool` through the rescue, `identity`/`io.debug` for
      `fn(a) -> a`, one row per declaration at its latest version — but whole requests take 0.4–10 s
      because the *text* tiers still run on type-shaped strings and the trigram/tsvector ones time out
      (`pog` query timeout) on `fn(a) -> a` or `Error`. **Done the same day**: a query that is
      *structurally* a type (`handlers.structural` — a `Fn`, `Tuple`, `Record`, `Union` or a `Named`
      with arguments; a bare `Named`, `Var` or `Hole` is not, so `Connection` keeps its name search)
      stops after the type tier; the response carries `"narrowed": true`, and `GET /search?q=…&broad=true`
      runs every tier anyway — the "élargir la recherche" switch venus still has to expose (follow-up:
      a toggle in venus, shown when `narrowed` is true). Measured end to end on the local stack: typed
      queries went from 0.4–10 s to 32–334 ms (`fn(a) -> a` 10.6 s → 39 ms, `fn(List(Int)) -> Bool`
      652 → 56 ms, the map query 3.8 s → 334 ms); bare names unchanged (`Connection` 319 ms, `Error`
      587 ms, all six tiers). The text tiers' own plans (trigram on code-shaped strings, `module_search`
      without an index nor a LIMIT — 1,958 rows for "Error") are untouched. Two gotchas met on the way: `pog` sends untyped parameters, so `cardinality($2)` fails with
      "could not determine polymorphic type" — cast (`$2::text[]`) every array parameter used outside a
      comparison with a typed column; and jupiter's boot is flaky on this laptop under load
      (`FailedToStartChild(2, InitFailed("shutdown"))` from mist's handler factory, 1 start in 8 at the
      worst moment, clean the next minute) — environmental, retried by the harness. Corpus facts: 8,193 distinct names; `String` 160k, `List` 113k,
      `Attribute` 80k, `Result` 69k, `Int` 64k rows; 24,513 rows carry an empty fingerprint
      (`fn(a) -> a` shapes). The rescue is still unmeasured. Design context kept below.
      The core fact: `type_names @> ARRAY[...]` requires the candidate to mention
      *every* query name — sound for `Exact` and for the query-more-general direction of `Lenient`
      (query holes add no names, so `_` never breaks `@>`), **unsound when the candidate is more
      general**: `list.is_empty : fn(List(a)) -> Bool` lacks `Int` for query `(List<Int>) -> Bool`.
      Degenerate case: `fn(a) -> a`, fingerprint `{}`. What was settled (Hoogle-in-SQL, two stages):
      - **Always `@>`** as the GIN workhorse. The fingerprint-*equality* filter (`type_names = $2`
        for hole-free queries) was considered and **withdrawn**: equality decomposes to
        `@> AND cardinality = $n` if ever needed, and the canonical-string lookup (below) serves
        the exact case better.
      - **Rank, don't hard-filter**: `WHERE arity = $1 AND type_names @> $2
        ORDER BY cardinality(type_names) - $n LIMIT 100`, then Gleam verify (`data/ir/unify`) on
        the ~100, truncate to 20. Cardinality measures how much the query's vars/holes had to
        absorb — `fn(a) -> a` (cardinality 0) surfaces *first* for query `fn(_) -> _`. Stored
        column (`type_names_count`, btree on `(arity, type_names_count)`) vs computed
        `cardinality()`: undecided, measure.
      - **Candidate-more-general rescue** (the direction `@>` drops), two sound options, measure:
        `OR has_variables` (coarse — passes every generic row; boolean derived from the IR at
        ingestion, keeps the everything-derives-from-the-IR invariant) vs `type_names <@ $2`
        (sharp — exactly the candidates whose concrete names all appear in the query; only valid
        for hole-free queries, since a query `_` can hide arbitrary candidate names, and `<@` is
        one of GIN's weak operators). The pre-Hoogle options recorded here earlier (`arity` alone,
        `&&` overlap) are superseded by these.
      - **UI consequence, decide at wiring time**: Hoogle users will type `a` meaning `_`; the
        lenient tier must stay visible under the exact tier (labelled result blocks, not one
        truncated list), or exact-tier hits will bury the specializations forever.
      Was true until this landed; `type_names` is now queried live by `fingerprint_candidates`.
- [x] **`fingerprint` arity bug — `TypeAlias` was always 0.** `ir.fingerprint` returned
      `#(0, names(aliased))` for aliases, while `unify_declaration` matches the query against
      `aliased`: an alias of a function type (`type Callback = fn(Int) -> Bool`) was dropped by
      `arity = 1` in phase 1 yet would verify in phase 2. Fixed: aliases go through
      `fingerprint_type(aliased)` like `Constant`; backfill of existing rows from the stored IR in
      migration `20261026120000` (`jsonb_array_length(signature_ir -> 'aliased' -> 'params')` when
      the aliased kind is `fn`, else 0). — **done**
- [ ] **Name-frequency table (Hoogle's rare-names insight).** A materialized `name → count` over
      the corpus' `type_names` (one GROUP BY, refreshed at ingestion or by a worker; derived from
      the IR like everything else). Useless while phase 1 is `@>`-only — containment is an
      intersection, so the rarest query name dominates its selectivity automatically. Becomes
      load-bearing the day `<@`/`&&` or signal-weighted ranking enter: prune `&&` arrays down to
      the query's rare names, decide dynamically between `<@` and the `arity + has_variables`
      fallback from the query's rarest name, and weight the cardinality `ORDER BY` by
      `-log(frequency)` so surviving *signal* ranks above surviving *noise*.
- [ ] `data/ir/parse` — query-side parser (`String → Type`) for user-typed queries. **Gleam syntax
      landed** (`packages/galaxy/src/data/ir/parse.gleam`): lexes with `glexer`, parses with `chomp`,
      lowers the Gleam surface (`fn(Result(a, b), fn(a) -> c) -> Result(c, b)`, `List(a)`,
      `#(Int, String)`, `module.Upper`) to the IR — lower-case ⇒ `Var`, `_` ⇒ `Hole` (each `_`
      independent) — then alpha-renames to first-appearance indices in its own name-keyed pass
      (`ir.alpha_rename` keys on `index`, which the parser has not assigned yet, so it cannot be
      reused as-is). Placed in **galaxy, not jupiter**, so venus reuses it client-side; galaxy gained
      `glexer` + `chomp`, both of which cross to the JS target (verified: `venus` builds on
      `--target javascript`). Still deferred:
  - [ ] The **canonical `$n`-notation** parser (inverse of `to_string`) — needed for the
        `to_string`/`parse` round-trip law and the canonical-string `Exact` shortcut. Our Gleam
        parser reads Gleam syntax, not `$n`, so those two do not round-trip yet.
  - [ ] Other-language query syntaxes (TS, …), alongside `from_ts`.
  - [ ] The **duplicate-invariant enforcement** the matcher assumes — no duplicate `Union` members,
        no duplicate `Record` field names (`equal_set`/`by_name` compare sets by length + membership
        and are fooled by multisets — `Int | Int` would "equal" `Int | String`). **Not yet needed**:
        the Gleam surface produces neither `Union` nor `Record`. Becomes load-bearing the moment the
        canonical/TS parser can emit them.
- [x] Remove old trie/actor type search (`jupiter/gleam/type_search*`, `search`, `cell` usage,
      `ctx.search`, worker registration); it is superseded by fingerprint + verify. Also removed
      with it: the old `jupiter/gleam/parse` text parser (only the trie used it), the
      `queries.exact_type_search` id lookup, the `type_search.add` call at ingestion, and jupiter's
      `cell`/`glexer`/`chomp` dependencies (galaxy keeps the latter two for `data/ir/parse`).
      `jupiter_test` no longer needs a database. — **done**

## Deferred — matching tier (tolerant)

- [ ] **Name synonyms / concept table.** `Int`≈`int`≈`Integer`, `String`≈`str`≈`string`.
      Fills `Name.concept`. One-to-many cases (`number`→{Int,Float}) stay here, never in the
      exact form. Location undecided: static table in code vs DB table joined at ingestion.
      **Product intent settled (2026-07-17): search is cross-language** — one query, all
      ecosystems. Two consequences, deferred but binding: this table is a **prerequisite of
      `from_ts`** (schedule them together, not concept-after); and it reaches phase 1 —
      `type_names` must store concept-expanded names (or a second concept column with its own
      GIN), otherwise the fingerprint drops every cross-language candidate before phase 2 ever
      sees it. Bare-name matching stays correct for same-language pairs in the meantime.
- [ ] **Wrapper transparency.** Optional async/effect-agnostic search: a bare `-> B` also
      surfaces `-> Promise<B>` / `-> Future<B>` / `-> Option(B)` by unwrapping known wrappers.
      Same mechanism gives `Option`-transparency. Lower precision than exact.
- [ ] **Nominal subtyping.** Declared `extends`/`implements`. Needs an external **subtype-edge
      table** (`Dog <: Animal`) populated at ingestion + a unifier rule that walks the edges.
- [ ] **Structural subtyping.** TS object shapes: a candidate `Record` matches a query `Record`
      when it contains *at least* the query's fields (field superset). No table needed.
- [ ] **Union widening.** The union analog of field-superset: a candidate `(Int | String | Bool)
      -> X` can serve query `(Int | String) -> X` (it accepts a superset of inputs). Unions
      currently match as equal-cardinality sets in *every* tier, so this "candidate more general"
      case is invisible to `Lenient` despite being exactly its mandate.
- [ ] **Repeated flexible variable vs nested `_`.** `unify`'s `bind` checks repeated-variable
      consistency with structural `equal`, where `equal(Hole, X) = False`: query
      `(Result<_, Int>, Result<String, Int>) -> X` vs candidate `fn(a, a) -> X` is *rejected*,
      although each position matches individually and the user's `_` said "anything here".
      Decide whether `equal` should be hole-tolerant; currently undiscussed and untested either way.

## Deferred — front-end lowering (per language, deterministic)

- [ ] **Nullability convergence.** `Option(a)` (Gleam) ≡ `t | null` / `t?` (TS) ≡ `Optional[a]`
      (Python) — front-ends emit the nominal `Option` form. No `Optional` node.
- [ ] **Intra-language synonym lowering (TS).** `number[]` and `Array<number>` are two syntaxes of
      one type — `from_ts` must emit the same `Named("Array", …)` for both (same for other aliased
      builtin spellings). A deterministic front-end rule, distinct from the cross-language concept
      table above.
- [ ] **No `Hole` in stored IR — keep the invariant true.** `unify` assumes holes only ever appear
      on the query side (a candidate `Hole` matches nothing in any tier); nothing enforces it. The
      TS front-end must **not** lower `any`/`unknown` to `Hole` — a fresh `Var` (or a decision of
      its own) when TS lands.
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

- [x] Stop storing `ref`: stripped from existing JSON + no longer emitted (migration
      `20260703100001`, `type_to_json`). Decoder made tolerant.
- [x] Dismantle the ingestion ref-resolution machinery: removed `extract_parameters_relation` +
      6 DB helpers from `generate/types`, the retry/`ignore` "restart extraction" flag in
      `tasks/hex`, and `Context.ignore_parameters_errors`. Ingestion no longer hits the DB to
      resolve refs.
- [x] Purified the `generate/types` chain: dropped the vestigial `ctx` everywhere (+ `Context`
      import); `api/signatures` call sites updated. Build is warning-clean. `Loss`/`Set` kept
      (always `Ok`/empty) to leave `api/signatures`' `result.try` wrappers untouched.
- [x] Dropped the write-only `parameters uuid[]` column (migration `20260703100002`) + its `upsert`
      param/INSERT/`pog.array` ($-placeholders renumbered). `generate/types` is now pure
      `pi.Type → Json` — `Loss`/`Set`/`reduce_components` removed (the chain can't fail), call sites
      in `api/signatures` de-`result.try`'d. Build warning-clean.
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

**Single stored structured representation — the faithful IR — everything derives from it.**
Columns on `package_type_fun_signature`, all derived from `signature_` (the source of truth):

- `signature_` — source string. Ground truth + FTS (`signature_search`, `documentation_search`). **Unchanged.**
- `signature_compact` — space-stripped `signature_`; `content_search` trigram tier. **Unchanged.**
- `signature_ir` — **new**, jsonb; the faithful alpha-renamed IR (`Var(index, name)`). The single
  structured source: display renders it, verify (phase 2) unifies it, the fingerprint derives from it.
- `arity int` + `type_names text[]` — **new**; fingerprint derived from the IR; GIN index on `type_names`.
- metadata columns (`name`, `documentation`, `kind`, `deprecation`, `implementations`, `metadata`) —
  **unchanged**, used by search filters/FTS; the IR is structural only and never duplicates them.
- `signature_json` — **to retire** once display reads the IR (venus renders `signature_ir`). Interim only.

Done already on the way here: `json_signature` → `signature_json` rename + `ref` strip (migration
`20260703100001`); write-only `parameters` column dropped (`20260703100002`). **Run `dbmate up`.**

- [ ] **Canonical-string shortcut for `Exact`** (supersedes the earlier "debug column only if
      wanted" note). Since the IR is alpha-renamed at ingestion, alpha-equivalence on a hole-free,
      fully-labelled query is **string equality**: a btree lookup on a stored canonical string
      short-circuits both phases for the most common query shape. Prerequisite: `to_string` must
      become a *normal form*, which is **four** changes, not the three originally listed — sort
      `Union` members and `Record` fields (the unifier treats them as sets, the printer prints
      list order), normalize `is_method` (one type prints two ways: `X :: (…) -> Y` vs
      `(X, …) -> Y`), strip labels (two columns: display string vs matching string), and **drop
      the `module.` qualification** (the printer emits `gleam/option.Option`, the unifier compares
      bare `name` — this fourth one was missed in the original list). Queries with `_` or omitted
      labels still go through fingerprint + verify; this is a shortcut, not a replacement.
- [x] Backfill existing rows (keyset): `tasks/backfill_ir` re-derives `signature_ir` + fingerprint from the
      stored `package_interface` of each release through `api/signatures.structured` (now public), one
      transaction per release, matched on `(package_module_id, name, kind)`. Run on the local snapshot
      on 2026-10-26 (`gleam run -m tasks/backfill_ir` with the real binaries, not the mise shims):
      502,188 of 502,237 rows filled in 3 min 26, 0 failed releases; the 49 rows left NULL (lustre_dev_tools
      2.0.0 and g18n 2.0.0, whose stored interface lacked ingested modules) were deleted from the local
      database so the Hex sync re-ingests them (both packages moved on Hex after the `hex_read` cursor;
      `lookup_release` fails on the missing release and extraction runs again); 3,481 interface
      declarations had no row. — **done**
- [ ] Multi-language: populate `package.language` / `package.registry` (migration `20260703100000`) as non-Gleam lands.
