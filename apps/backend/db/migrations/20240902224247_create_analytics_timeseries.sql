-- migrate:up
create table analytics_timeseries (
  query text not null,
  occurences int not null,
  date timestamptz not null,
  primary key (query, date)
);

-- migrate:down
drop table analytics_timeseries;
