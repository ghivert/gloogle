-- migrate:up
create extension if not exists "fuzzystrmatch";

-- migrate:down
drop extension if exists "fuzzystrmatch";
