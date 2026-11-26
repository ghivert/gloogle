-- migrate:up
-- Every search tier ordered releases by a semver array computed per row —
-- `string_to_array(regexp_replace(version, …), '.')::int[]` — and so did the
-- type tier's `DISTINCT ON` to the latest version. On a 33k-row candidate set
-- that regexp alone was measured at ~385 ms, the single largest cost of a
-- frequent-name query. A release's version never changes, so the array is a
-- generated stored column: derived by Postgres at insert time, no ingestion
-- code touched, backfilled by the ALTER itself.
alter table package_release
  add column version_ordering integer[]
    generated always as (
      string_to_array(regexp_replace(version, '([0-9]+).([0-9]+).([0-9]+).*', '\1.\2.\3'), '.')::integer[]
    ) stored;

-- migrate:down
alter table package_release
  drop column version_ordering;
