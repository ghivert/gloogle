-- migrate:up
-- `signature_json` was the display form of a declaration, the structured
-- duplicate of `signature_ir` with per-node widths precomputed for venus. Venus
-- renders `signature_ir` since 2026-10-27 and nothing reads this column any
-- more; the IR is the single structured representation (`signature-ir.md`,
-- "Single stored representation"). ~900 bytes per row, 500k rows: run a
-- `VACUUM` afterwards to reclaim the space.
alter table package_type_fun_signature drop column signature_json;

-- migrate:down
-- Re-adding the column cannot restore its content; re-ingesting would, if the
-- writer (`jupiter/gleam/generate/types`, removed with this migration) were
-- restored too. Nullable on the way back, since nothing writes it.
alter table package_type_fun_signature add column signature_json jsonb;
