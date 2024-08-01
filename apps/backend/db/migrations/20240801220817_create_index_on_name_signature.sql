-- migrate:up
create index package_name_index on package_type_fun_signature (name, kind);

-- migrate:down
drop index package_name_index;
