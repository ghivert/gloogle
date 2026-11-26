-- migrate:up
-- The faithful, alpha-renamed signature IR (`data/ir`), stored as jsonb. This is
-- the single structured representation: display, verify (phase-2 matching), and
-- the fingerprint all derive from it. Nullable for now — ingestion populates it
-- going forward; existing rows are backfilled in a later step. Once venus renders
-- the IR, `signature_json` is retired.
alter table package_type_fun_signature
  add column signature_ir jsonb;

-- migrate:down
alter table package_type_fun_signature
  drop column signature_ir;
