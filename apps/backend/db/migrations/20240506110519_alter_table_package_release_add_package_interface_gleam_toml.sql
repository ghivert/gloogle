-- migrate:up
alter table package_release
  add column package_interface text,
  add column gleam_toml text;

-- migrate:down
alter table package_release
  drop column package_interface,
  drop column gleam_toml;
