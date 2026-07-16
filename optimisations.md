# Jupiter SQL optimisations

## Point 7 — `find_similar_type_names`: unindexed Levenshtein

**Status:** not started (low priority).

`find_similar_type_names` ([queries.gleam](apps/jupiter/src/jupiter/postgres/queries.gleam)) unions a hard-coded
`VALUES` list of built-in types with rows from `package_type_fun_signature` filtered by
`levenshtein_less_equal(name, $1, 2) <= 2`, then `DISTINCT ON (name)`.

Baseline: **~22.8 ms**, index-only scan on `package_name_index` but filtering ~75k rows through Levenshtein (which is
not directly indexable).

Options:

- **Pre-filter with the point 5 trigram index** (`name % $1`, the `pg_trgm` similarity operator) before applying
  Levenshtein, to shrink the scanned set. ⚠️ This is also a **behaviour change**: the default similarity threshold (0.3)
  can drop valid Levenshtein-2 neighbours for short names.
- **Cleanup (safe):** the trailing `<= 2` is redundant — `levenshtein_less_equal(_, _, 2)` already caps the distance
  at 2.

Priority: low. Revisit only if it becomes a bottleneck after point 5.

## Open behaviour question — signature matching in `content_search`

**Not an optimisation — a precision-vs-recall product decision.** The `signature_compact` column and its index are
identical either way; only one line of the query changes, so this can be decided later without touching the migration.

|                           | **A — Wildcard** (current)                              | **B — Contiguous** (proposed)                                           |
| ------------------------- | ------------------------------------------------------- | ----------------------------------------------------------------------- |
| SQL                       | `ILIKE '%' \|\| replace($2, ' ', '%') \|\| '%'`         | `ILIKE '%' \|\| replace($1, ' ', '') \|\| '%'`                          |
| Query normalisation       | `( ) , .` → spaces → `%` (via `transform_query`)        | strip spaces only (symmetric with the column)                           |
| Precision                 | low: `fn(Int, Int)` also matches `fn(Int, String, Int)` | high: only the exact structure matches                                  |
| Recall / typing tolerance | high: `fn Int Int` matches `fn(Int, Int)`               | stricter: user must type `(`, `,`                                       |
| Trigram index usage       | under-used (discards punctuation trigrams)              | optimal (dense pattern, structural trigrams)                            |
| Side effects              | —                                                       | drops `transform_query`/`$2`; `given.not` becomes literal (`.` matters) |

Core question: a **tolerant** search (type loosely, cast a wide net, accept super-set matches) or a
**structure-faithful** one (what you type is the shape you get, precise results)? Real top queries are structured
(`fn(Int, Int) -> Order` is #1), which argues for B, but A protects users who type without parentheses.

Current query is in mode **A**.
