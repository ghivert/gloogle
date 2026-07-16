-- migrate:up
-- First step towards multi-language support. Every package currently indexed is
-- a Gleam package hosted on Hex, so both columns default accordingly and
-- backfill existing rows. Later, when NPM/PyPI/Maven are indexed, these two
-- indicators are all that's needed to disambiguate and boost same-language
-- results — the rest of the schema (release/version/module/signature) already
-- generalises across ecosystems.
alter table package
  add column language text not null default 'gleam',
  add column registry text not null default 'hex';

-- migrate:down
alter table package
  drop column language,
  drop column registry;
