-- migrate:up
create table package_version (
  id integer primary key generated always as identity,
  package_id int references package(id),
  version text not null
);

-- migrate:down
drop table package_version;
