-- migrate:up
alter table package
  add column rank int not null default 0,
  add column favorites int not null default 0;

-- migrate:down
alter table package
  drop column rank,
  drop column favorites;
