import backend/data/hex_read.{type HexRead, HexRead}
import backend/data/hex_user.{type HexUser}
import backend/index/decoders
import backend/index/error
import backend/index/helpers
import birl.{type Time}
import gleam/bool
import gleam/dynamic
import gleam/hexpm
import gleam/io
import gleam/list
import gleam/pgo
import gleam/result

pub fn get_last_hex_date(db: pgo.Connection) {
  "SELECT id, last_check FROM hex_read ORDER BY last_check DESC LIMIT 1"
  |> pgo.execute(db, [], decoders.hex_read)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(response) {
    response.rows
    |> list.first()
    |> result.unwrap(HexRead(0, birl.from_unix(0)))
    |> fn(h: HexRead) { h.last_check }
  })
}

pub fn upsert_most_recent_hex_timestamp(db: pgo.Connection, latest: Time) {
  let timestamp = helpers.convert_time(latest)
  "INSERT INTO hex_read
   VALUES (1, $1)
   ON CONFLICT (id) DO UPDATE
     SET last_check = $1
   RETURNING *"
  |> pgo.execute(db, [timestamp], decoders.hex_read)
  |> result.map_error(io.debug)
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(response) {
    let err = "Upsert most recent hex timestamp failed"
    response.rows
    |> list.first()
    |> result.replace_error(error.UnknownError(err))
  })
}

pub fn upsert_hex_user(db: pgo.Connection, owner: hexpm.PackageOwner) {
  let username = pgo.text(owner.username)
  let email = pgo.nullable(pgo.text, owner.email)
  let url = pgo.text(owner.url)
  "INSERT INTO hex_user (username, email, url)
   VALUES ($1, $2, $3)
   ON CONFLICT (username) DO UPDATE
     SET email = $2, url = $3
   RETURNING id, username, email, url, created_at, updated_at"
  |> pgo.execute(db, [username, email, url], decoders.hex_user)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

fn upsert_package_owners(db: pgo.Connection, owners: List(hexpm.PackageOwner)) {
  owners
  |> list.map(upsert_hex_user(db, _))
  |> result.all()
  |> result.map(list.flatten)
}

fn get_current_package_owners(db: pgo.Connection, package_id: Int) {
  let pid = pgo.int(package_id)
  "SELECT package_owner.hex_user_id
   FROM package_owner
   WHERE package_owner.package_id = $1"
  |> pgo.execute(db, [pid], dynamic.element(0, dynamic.int))
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

fn add_new_package_owners(
  db: pgo.Connection,
  owners: List(HexUser),
  current_owners: List(Int),
  package_id: Int,
) {
  owners
  |> list.filter(fn(o) { bool.negate(list.contains(current_owners, o.id)) })
  |> list.map(fn(u) {
    let hex_user_id = pgo.int(u.id)
    let pid = pgo.int(package_id)
    "INSERT INTO package_owner (hex_user_id, package_id)
     VALUES ($1, $2)"
    |> pgo.execute(db, [hex_user_id, pid], dynamic.dynamic)
  })
  |> result.all()
  |> result.map_error(error.DatabaseError)
}

fn remove_old_package_owners(
  db: pgo.Connection,
  owners: List(HexUser),
  current_owners: List(Int),
  package_id: Int,
) {
  let curr = list.map(owners, fn(o) { o.id })
  current_owners
  |> list.filter(fn(id) { list.contains(curr, id) })
  |> list.map(fn(u) {
    let hex_user_id = pgo.int(u)
    let pid = pgo.int(package_id)
    "DELETE FROM package_owner
     WHERE package_owner.hex_user_id = $1
       AND package_owner.package_id = $2"
    |> pgo.execute(db, [hex_user_id, pid], dynamic.dynamic)
  })
  |> result.all()
  |> result.map_error(error.DatabaseError)
}

pub fn sync_package_owners(
  db: pgo.Connection,
  package_id: Int,
  owners: List(hexpm.PackageOwner),
) {
  use news <- result.try(upsert_package_owners(db, owners))
  use curr <- result.try(get_current_package_owners(db, package_id))
  use _ <- result.try(add_new_package_owners(db, news, curr, package_id))
  use _ <- result.try(remove_old_package_owners(db, news, curr, package_id))
  Ok(Nil)
}

pub fn upsert_package(db: pgo.Connection, package: hexpm.Package) {
  let name = pgo.text(package.name)
  let repo = pgo.nullable(pgo.text, package.html_url)
  let docs = pgo.nullable(pgo.text, package.docs_html_url)
  "INSERT INTO package (name, repository, documentation)
   VALUES ($1, $2, $3)
   ON CONFLICT (name) DO UPDATE
     SET repository = $2, documentation = $3
   RETURNING id"
  |> pgo.execute(db, [name, repo, docs], dynamic.element(0, dynamic.int))
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(response) {
    response.rows
    |> list.first()
    |> result.replace_error(error.UnknownError("Upsert package error"))
  })
}

pub fn upsert_release(
  db: pgo.Connection,
  package_id: Int,
  release: hexpm.Release,
) {
  let package_id = pgo.int(package_id)
  let version = pgo.text(release.version)
  let url = pgo.text(release.url)
  "INSERT INTO package_release (package_id, version, url)
   VALUES ($1, $2, $3)
   ON CONFLICT (package_id, version) DO NOTHING"
  |> pgo.execute(db, [package_id, version, url], dynamic.dynamic)
  |> result.map_error(error.DatabaseError)
}
