-- migrate:up
create index package_type_fun_signature_name on package_type_fun_signature (name);
create index package_type_fun_signature_signature on package_type_fun_signature using gin (to_tsvector('english', signature_));

-- migrate:down
drop index package_type_fun_signature_name;
drop index package_type_fun_signature_signature;
