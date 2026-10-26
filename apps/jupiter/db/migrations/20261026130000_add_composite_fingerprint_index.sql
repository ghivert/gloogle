-- migrate:up
-- Phase 1 of type search filters `WHERE arity = $1 AND type_names @> $2`. The
-- single-column GIN answered the containment but left `arity` to a heap filter
-- that discarded 60–80 % of the rows fetched. `btree_gin` lets an integer sit in
-- a GIN, so the composite index puts both in the index condition. Measured on
-- the 502k-row snapshot (2026-10-26): the cascade query drops from 128 ms to
-- 44 ms on `{List}`, 120 → 22 ms on `{Int}`, 14 → 2.5 ms on `{Decoder}`; the
-- index is 5 MB and builds in under 2 s. A multicolumn GIN also serves queries
-- on `type_names` alone, so the old index is redundant and dropped.
create extension if not exists btree_gin;

create index package_type_fun_signature_arity_type_names_gin
  on package_type_fun_signature using gin (arity, type_names);

drop index package_type_fun_signature_type_names_gin;

-- migrate:down
create index package_type_fun_signature_type_names_gin
  on package_type_fun_signature using gin (type_names);

drop index package_type_fun_signature_arity_type_names_gin;
