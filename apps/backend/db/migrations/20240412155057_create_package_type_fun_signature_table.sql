-- migrate:up
create type type_nature
  as enum (
    'function',
    'type_definition',
    'type_alias',
    'constant'
  );

create table package_type_fun_signature (
  -- Primary key
  id integer primary key generated always as identity,

  -- Data, used in search and display mostly.
  -- Keeping the source code signature, reconstructed from the package interface
  --  allows to search in it directly, with full text search.
  -- Name, documentation, nature and metadata directly comes from package interface.
  name text not null,
  documentation text not null,
  signature_ text not null,
  json_signature jsonb not null,
  nature type_nature not null,
  parameters int[] not null,
  deprecation text,
  metadata jsonb not null,

  -- Where is located the signature.
  -- Module is simply the module name from the package.
  -- Package can be retrieved through the package release.
  package_module_id int references package_module(id),

  -- Metadata on the row itself.
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null,
  unique (package_module_id, name)
);

create trigger package_type_fun_signature_moddatetime
  before update on package_type_fun_signature
  for each row
  execute procedure moddatetime (updated_at);

-- migrate:down
drop trigger package_type_fun_signature_moddatetime on package_type_fun_signature;
drop table package_type_fun_signature;
drop type type_nature;
