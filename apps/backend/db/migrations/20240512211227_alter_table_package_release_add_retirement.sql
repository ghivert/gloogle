-- migrate:up
alter table package_release
  add column retirement jsonb;

-- migrate:down
alter table package_release
  drop column retirement;
