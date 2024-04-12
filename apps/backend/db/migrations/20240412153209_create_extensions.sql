-- migrate:up
create extension if not exists "uuid-ossp";
create extension if not exists "moddatetime";

-- migrate:down
drop extension if exists "uuid-ossp";
drop extension if exists "moddatetime";
