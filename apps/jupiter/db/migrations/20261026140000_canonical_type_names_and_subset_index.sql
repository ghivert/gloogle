-- migrate:up
-- Phase 1 could only find candidates *at least* as specific as the query
-- (`type_names @> query`); a candidate more general than the query — the
-- `fn(List(a)) -> Bool` that should answer `(List(Int)) -> Bool` — was dropped
-- before verification ever saw it. Such a candidate's concrete names form a
-- subset of the query's: one of the 2^n sub-arrays of the query's n names, so
-- a handful of equality probes. `<@` on the GIN was measured at 75–320 ms (it
-- rechecks every row sharing a name), and array equality is claimed by the GIN
-- too (`=` is in its opclass, with the same recheck). Hence a *text* key the
-- GIN cannot serve: `type_names_key`, the sorted names joined by `,`, derived
-- from the IR like `type_names` itself and written at every call site
-- (`queries.upsert_package_type_fun_signature`, `tasks/backfill_ir`).
--
-- Three steps: make every stored fingerprint canonical (sorted — `data/ir`
-- now sorts too), derive the key, index `(arity, type_names_key)`. The mass
-- update bloats the composite GIN's pending list, so it is reindexed last.
update package_type_fun_signature
  set type_names = coalesce((select array_agg(n order by n) from unnest(type_names) n), '{}')
  where type_names is not null
    and type_names <> coalesce((select array_agg(n order by n) from unnest(type_names) n), '{}');

alter table package_type_fun_signature
  add column type_names_key text;

update package_type_fun_signature
  set type_names_key = array_to_string(type_names, ',')
  where type_names is not null;

create index package_type_fun_signature_arity_type_names_key_btree
  on package_type_fun_signature (arity, type_names_key);

reindex index package_type_fun_signature_arity_type_names_gin;

-- migrate:down
-- Arrays stay sorted (a sorted set is still the same set); the key goes.
drop index package_type_fun_signature_arity_type_names_key_btree;

alter table package_type_fun_signature
  drop column type_names_key;
