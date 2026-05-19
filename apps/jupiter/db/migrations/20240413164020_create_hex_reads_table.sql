-- migrate:up
create table hex_read (
  id uuid primary key default uuidv7(),
  last_check timestamptz default now() not null
);

-- migrate:down
drop table hex_read;
