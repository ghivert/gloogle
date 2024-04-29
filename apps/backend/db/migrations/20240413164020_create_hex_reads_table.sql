-- migrate:up
create table hex_read (
  id integer primary key generated always as identity,
  last_check timestamptz default now() not null
);

-- migrate:down
drop table hex_read;
