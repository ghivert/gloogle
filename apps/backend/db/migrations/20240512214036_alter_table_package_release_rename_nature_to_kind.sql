-- migrate:up
alter table package_type_fun_signature
  rename column nature to kind;

-- migrate:down
alter table package_type_fun_signature
  rename column kind to nature;
