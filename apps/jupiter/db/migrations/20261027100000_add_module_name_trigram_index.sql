-- migrate:up
-- `module_search` is the last tier of the cascade: `m.name LIKE '%q%'` over
-- `package_module`. It had no index at all, so every search ended with a
-- sequential scan of the module table (33k rows) hash-joined to every release.
-- `pg_trgm` is already installed for `content_search`; a trigram GIN on the
-- module name serves `LIKE '%…%'` directly. Measured on the 502k-row snapshot
-- (2026-10-27, warm cache): the tier's query on "error" drops from 45 ms to
-- 10 ms, the seq scan + hash join becoming a bitmap scan + nested loop.
create index package_module_name_trgm
  on package_module using gin (name gin_trgm_ops);

-- migrate:down
drop index package_module_name_trgm;
