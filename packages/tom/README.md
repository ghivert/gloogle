# tom (vendored)

A copy of [`tom`](https://github.com/lpil/tom) **2.1.0** by Louis Pilfold, the
pure Gleam TOML parser, with one fix. Licence: Apache-2.0, as declared in the
upstream `gleam.toml` (the upstream repository ships no licence file).

- `src/` and `test/tom_test.gleam` are the `v2.1.0` tag (identical to the Hex
  tarball, outer checksum `DCF04CB7…`), plus the patch below.
- jupiter depends on it by path (`apps/jupiter/gleam.toml`).

## The patch

TOML lets a super-table be defined after one of its sub-tables:

```toml
[dev-dependencies.envoy]   # creates `dev-dependencies` implicitly
version = ">= 1.0.2"

[dev-dependencies]         # defines it, once
gleeunit = ">= 1.0.0"
```

tom 2.1.0 rejects this with `KeyAlreadyInUse(["dev-dependencies"])`. Gleam
accepts it, so it reaches Hex: lumenmail 1.1.2 failed to ingest because of it.

The fix, in `src/tom.gleam` (search for "Gloogle patch"):

- `parse_tables` remembers the `[table]` headers it has seen: the same header
  twice is still `KeyAlreadyInUse`;
- `merge` merges a `[table]` into a table created implicitly; a key present on
  both sides is still `KeyAlreadyInUse` (with its full path).

Tests: the 141 upstream tests unchanged, plus five at the end of
`test/tom_test.gleam` (the valid order, lumenmail's `gleam.toml`, duplicate
headers, conflicting keys, array-of-tables). Erlang and JavaScript.

Known leniency kept: a table created by dotted keys (`a.b = 1`) can still be
reopened by `[a]`, which TOML forbids. Nothing in Gloogle depends on it.

## Leaving the fork

Once a tom release fixes this upstream, replace the path dependency in
`apps/jupiter/gleam.toml` by the Hex version and delete this directory.
