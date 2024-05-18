-- migrate:up
create index package_type_fun_signature_documentation on package_type_fun_signature using gin (to_tsvector('english', documentation));

-- migrate:down
drop index package_type_fun_signature_documentation;
