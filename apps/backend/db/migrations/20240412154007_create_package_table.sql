-- migrate:up
create table package (
  id integer primary key generated always as identity,
  name text unique not null,
  repository text,    -- Extracted from gleam.toml metadata, pushed on hex fortunately.
  documentation text, -- URL to documentation, something like https://hexdocs.pm/package.
  hex_url text,       -- URL to package, something like https://hex.pm/package.
  links jsonb,        -- All links defined in gleam.toml. Defined in hex metadata.
  licenses jsonb,     -- All licenses defined in gleam.toml. Defined in hex metadata.
  description text,   -- Description provided on hex.
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
