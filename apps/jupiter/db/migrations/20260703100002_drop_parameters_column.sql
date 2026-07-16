-- migrate:up
-- The `parameters uuid[]` column held ingestion-time-resolved cross-reference
-- ids. It was write-only (no query reads it) and is obsolete now that refs
-- resolve at query time. Dropping it lets ingestion stop resolving refs.
alter table package_type_fun_signature
  drop column parameters;

-- migrate:down
alter table package_type_fun_signature
  add column parameters uuid[] not null default '{}'::uuid[];
