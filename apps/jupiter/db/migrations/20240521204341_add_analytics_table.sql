-- migrate:up
create table analytics (
  id uuid primary key default uuidv7(),
  foreign_id uuid not null,
  table_name text not null,
  content jsonb not null,
  day timestamptz not null,
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null,
  unique (foreign_id, table_name, day)
);

create trigger analytics_moddatetime
  before update on analytics
  for each row
  execute procedure moddatetime (updated_at);

-- migrate:down
drop trigger analytics_moddatetime on analytics;
drop table analytics;
