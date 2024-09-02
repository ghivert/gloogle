-- migrate:up
alter table only search_analytics alter column occurences set default 1;
update search_analytics set occurences = occurences + 1;

-- migrate:down
alter table only search_analytics alter column occurences set default 0;
update search_analytics set occurences = occurences - 1;
