-- migrate:up
create table analytics (
  id integer primary key generated always as identity,
  foreign_id int not null,
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
