-- migrate:up
create extension if not exists "pg_trgm";

-- Stored, space-stripped copy of `signature_`. `content_search` matches against
-- the signature with all spaces removed; materialising it as a generated column
-- lets a GIN trigram index cover those predicates (an index on the raw
-- `replace(signature_, ' ', '')` expression cannot be shared as cleanly and the
-- query now targets this column directly).
alter table package_type_fun_signature
  add column signature_compact text
    generated always as (replace(signature_, ' ', '')) stored;

-- Trigram indexes backing `content_search`'s remaining ILIKE/LIKE '%...%'
-- predicates. Every branch of the OR must be indexable, otherwise the planner
-- falls back to a full sequential scan to evaluate the disjunction.
create index package_type_fun_signature_name_trgm
  on package_type_fun_signature using gin (name gin_trgm_ops);
create index package_type_fun_signature_signature_compact_trgm
  on package_type_fun_signature using gin (signature_compact gin_trgm_ops);

-- migrate:down
drop index package_type_fun_signature_signature_compact_trgm;
drop index package_type_fun_signature_name_trgm;
alter table package_type_fun_signature drop column signature_compact;
drop extension if exists "pg_trgm";
