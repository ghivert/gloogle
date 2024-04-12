-- migrate:up
create table package (
  id integer primary key generated always as identity,
  name text not null,
  repository text,
  documentation text
);

-- migrate:down
drop table package;
