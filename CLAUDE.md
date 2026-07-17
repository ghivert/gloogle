# Gloogle

Monorepo. `apps/jupiter` (backend, Gleam/Erlang), `apps/venus` (frontend, Lustre),
`packages/galaxy` (types shared across the HTTP boundary), plus `plasma`, `chomp`, `vitools`.

Design docs: `signature-ir.md` (the signature IR design), `signature-ir-todos.md` (deferred work),
`optimisations.md`.

## Architecture documentation is not optional

`architecture.html` is the rendered map of the backend: supervision tree, ingestion pipeline,
signature IR, shared types, HTTP API, search cascade, relational schema, column-by-column
derivation, indexes. It is bilingual (FR/EN) and self-contained — no external dependencies.

**Any change that can alter the architecture MUST update `architecture.html` in the same change,
and MUST spell out what changed.** A diagram that lies is worse than no diagram: it is trusted,
and it is wrong. This page is read to answer "how does this work" — if it drifts from the code,
it silently teaches the wrong model.

### Triggers — touching any of these means the page is in scope

```
apps/jupiter/db/migrations/**          schema change, index, column, enum
apps/jupiter/db/schema.sql             the dumped schema
apps/jupiter/src/data/ir.gleam         IR node set, codec, fingerprint, alpha-rename
apps/jupiter/src/data/ir/**            language front-ends (from_gleam, future from_ts, …)
apps/jupiter/src/jupiter.gleam         supervision tree, workers, boot
apps/jupiter/src/jupiter/workers.gleam periodic worker set or cadence
apps/jupiter/src/jupiter/router.gleam  routes, middleware
apps/jupiter/src/jupiter/router/handlers.gleam   the search cascade, endpoint behaviour
apps/jupiter/src/jupiter/postgres/queries.gleam  SQL, search tiers, upsert columns
apps/jupiter/src/api/signatures.gleam  what gets derived and stored per declaration
packages/galaxy/src/data/**            the shared HTTP types
apps/jupiter/src/tasks/**              the ingestion pipeline
```

The trigger list is a floor, not a ceiling. **Use judgement on top of it**: if a change alters how
the system is shaped or how a reader should reason about it, update the page even when no path
above is touched. If a change touches a path above but is genuinely cosmetic (a rename with no
structural meaning, a log line), say so and move on — do not manufacture a diff.

### What "detail what changed" means

Not "updated the docs". State, in the response:

1. **What moved** — the concrete architectural delta (a column is now derived, a worker is gone,
   a tier changed order, a type crossed a package boundary).
2. **Which sections of the page changed**, and what they said before.
3. **What is now stale but not yet fixed**, if anything — an intentional gap is fine, a silent one
   is not.

### Non-negotiables when editing the page

- **Update BOTH languages.** Every block is duplicated `lang="fr"` / `lang="en"`; the CSS hides the
  inactive one. A block updated in one language only is a silent regression — the other language
  keeps asserting the old architecture. French is the CSS default (the page must stay readable with
  no JavaScript).
- **Keep the status tags honest.** `source de vérité` / `source of truth`, `dérivé` / `derived`,
  `à retirer` / `to remove`. They encode the single-stored-representation invariant; if a column
  changes status, the tag changes with it.
- **Self-contained.** Inline CSS/SVG only. No CDN, no external font, no remote asset.
- **Verify, don't eyeball.** After editing, assert that no block leaks into the wrong language
  (count visible `[lang="fr"]` vs `[lang="en"]` in both modes — they must match, with `<html lang>`
  as the only extra).

### Keep the design docs in sync too

`signature-ir.md` and `signature-ir-todos.md` are the prose companions. When a TODO lands, tick it
there as well — `architecture.html` shows the state, the todos file shows the trajectory.
