-- migrate:up
alter table package
  add column popularity jsonb,
  drop column favorites;

-- migrate:down
alter table package
  drop column popularity,
  add column favorites int not null default 0;
