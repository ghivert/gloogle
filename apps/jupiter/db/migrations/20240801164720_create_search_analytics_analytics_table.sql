-- migrate:up
create table search_analytics (
  query text primary key,
  occurences int not null default 0,
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null
);

create trigger search_analytics_moddatetime
  before update on search_analytics
  for each row
  execute procedure moddatetime (updated_at);

-- migrate:down
drop trigger search_analytics_moddatetime on search_analytics;
drop table search_analytics;
