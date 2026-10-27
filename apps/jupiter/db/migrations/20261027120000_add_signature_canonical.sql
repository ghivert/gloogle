-- migrate:up
-- The normal form of a declaration's type (`ir.to_canonical`: sets sorted,
-- labels, method form and module qualification dropped, variables renamed in
-- reading order), so that alpha-equivalence of a hole-free, label-free query
-- with a candidate is string equality. Derived from `signature_ir` like
-- `arity` and `type_names`; NULL for a `type_definition`, which never matches
-- by shape. The btree answers the exact bucket of the type tier with a few
-- probes whatever the names' frequency, and guarantees that an exact match is
-- never lost to phase 1's `LIMIT 100`. Filled by ingestion from now on and by
-- `tasks/backfill_ir` for existing rows.
alter table package_type_fun_signature add column signature_canonical text;

create index package_type_fun_signature_canonical_btree
  on package_type_fun_signature (signature_canonical)
  where signature_canonical is not null;

-- migrate:down
drop index package_type_fun_signature_canonical_btree;
alter table package_type_fun_signature drop column signature_canonical;
