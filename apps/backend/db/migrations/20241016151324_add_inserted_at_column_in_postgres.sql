-- migrate:up
alter table package_release add column inserted_at timestamp;

-- migrate:down
alter table package_release drop column inserted_at;
