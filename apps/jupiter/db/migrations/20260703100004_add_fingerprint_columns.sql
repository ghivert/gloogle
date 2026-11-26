-- migrate:up
-- Phase-1 fingerprint derived from `signature_ir`: `arity` (top-level function
-- parameter count) and `type_names` (the set of concrete nominal names the
-- signature mentions, nesting flattened). Candidate selection runs
-- `WHERE arity = $1 AND type_names @> ARRAY[...]`; the GIN index answers the
-- array containment. Nullable for now — populated going forward, existing rows
-- backfilled later.
alter table package_type_fun_signature
  add column arity integer,
  add column type_names text[];

create index package_type_fun_signature_type_names_gin
  on package_type_fun_signature using gin (type_names);

-- migrate:down
drop index package_type_fun_signature_type_names_gin;

alter table package_type_fun_signature
  drop column arity,
  drop column type_names;
