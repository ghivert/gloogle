-- migrate:up
create table hex_user (
  id integer primary key generated always as identity,
  username text unique not null,
  email text,
  url text not null,
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null
);

create trigger hex_user_moddatetime
  before update on hex_user
  for each row
  execute procedure moddatetime (updated_at);

-- migrate:down
drop trigger hex_user_moddatetime on hex_user;
drop table hex_user;
