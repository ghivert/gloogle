-- migrate:up
-- A release's declared dependencies, as its stored `gleam.toml` lists them:
-- one row per (release, package name, kind), the requirement kept verbatim
-- (`>= 1.0.0 and < 2.0.0`, `~> 4.1`). What the type links need: a `Named` in
-- a signature carries its package and module, and the version it refers to
-- is whichever release of that package satisfies *this* release's
-- requirement — resolved at query time against the releases known today,
-- never stored (a stored resolution goes stale at the next publish). A
-- normalised table rather than jsonb, for the joins and for the questions the
-- data can answer (who depends on what, which requirements are too tight).
-- Filled by ingestion from now on and by `tasks/backfill_dependencies` for
-- the 5.5k releases that already store their `gleam.toml`.
create type dependency_kind as enum ('dependency', 'dev-dependency');

create table package_dependency (
  id uuid primary key default uuidv7(),
  package_release_id uuid not null references package_release (id) on delete cascade,
  name text not null,
  requirement text not null,
  kind dependency_kind not null,
  unique (package_release_id, name, kind)
);

create index package_dependency_name on package_dependency (name);

-- migrate:down
drop table package_dependency;
drop type dependency_kind;
