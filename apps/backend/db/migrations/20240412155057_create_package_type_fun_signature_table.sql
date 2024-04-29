-- migrate:up
create table package_type_fun_signature (
  id integer primary key generated always as identity,
  package_release_id int references package_release(id),
  module text not null,
  name text not null,
  content text not null,
  comment text,
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null,
  unique (package_release_id, module, name)
);

create trigger package_type_fun_signature_moddatetime
  before update on package_type_fun_signature
  for each row
  execute procedure moddatetime (updated_at);

-- migrate:down
drop trigger package_type_fun_signature_moddatetime on package_type_fun_signature;
drop table package_type_fun_signature;
