-- migrate:up
create table signature (
  id integer primary key generated always as identity,
  package_id int references package(id),
  package_version_id int references package_version(id),
  name text not null,
  content text not null,
  comment text
);

-- migrate:down
drop table signature;
