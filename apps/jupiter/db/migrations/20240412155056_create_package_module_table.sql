-- migrate:up
create table package_module (
  id uuid primary key default uuidv7(),
  name text not null,
  documentation text not null,
  package_release_id uuid references package_release(id),
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null,
  unique (package_release_id, name)
);

create trigger package_module_moddatetime
  before update on package_module
  for each row
  execute procedure moddatetime (updated_at);

-- migrate:down
drop trigger package_module_moddatetime on package_module;
drop table package_module;
