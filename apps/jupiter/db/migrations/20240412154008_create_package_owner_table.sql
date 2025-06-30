-- Used to create a many-to-many relationship between package and owners.

-- migrate:up
create table package_owner (
  hex_user_id int references hex_user(id),
  package_id int references package(id),
  created_at timestamptz default current_timestamp not null,
  updated_at timestamptz default current_timestamp not null,
  primary key (hex_user_id, package_id)
);

create trigger package_owner_moddatetime
  before update on package_owner
  for each row
  execute procedure moddatetime (updated_at);

-- migrate:down
drop trigger package_owner_moddatetime on package_owner;
drop table package_owner;
