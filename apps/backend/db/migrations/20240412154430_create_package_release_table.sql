-- migrate:up
create table package_release (
  id integer primary key generated always as identity,
  package_id int references package(id),
  version text not null, -- Semver version
  url text not null,     -- Direct URL of the release.
  gleam_constraint text,
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null,
  unique (package_id, version)
);

create trigger package_release_moddatetime
  before update on package_release
  for each row
  execute procedure moddatetime (updated_at);

-- migrate:down
drop trigger package_release_moddatetime on package_release;
drop table package_release;
