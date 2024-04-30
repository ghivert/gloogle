-- migrate:up
create table package (
  id integer primary key generated always as identity,
  name text unique not null,
  repository text,
  documentation text,
  hex_url text,
  links jsonb,
  licenses jsonb,
  description text,
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null
);

create trigger package_moddatetime
  before update on package
  for each row
  execute procedure moddatetime (updated_at);

-- migrate:down
drop trigger package_moddatetime on package;
drop table package;
