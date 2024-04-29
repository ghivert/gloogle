import backend/data/hex_read.{type HexRead, HexRead}
import backend/data/hex_user.{type HexUser}
import backend/index/decoders
import backend/index/error.{type Error}
import backend/index/helpers
import birl.{type Time}
import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/hexpm
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pgo
import gleam/result
import gleam/string

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
   ON CONFLICT (email) DO UPDATE
     SET username = $1, email = $2, url = $3
   RETURNING id, username, email, url, created_at, updated_at"
  |> pgo.execute(db, [username, email, url], decoders.hex_user)
  |> result.map(fn(r) { r.rows })
}

fn upsert_package_owners(db: pgo.Connection, owners: List(hexpm.PackageOwner)) {
  owners
  |> list.map(upsert_hex_user(db, _))
  |> result.all()
  |> result.map(list.flatten)
}

fn get_current_package_owners(db: pgo.Connection, package: hexpm.Package) {
  "SELECT package_owner.hex_user_id
   FROM package_owner
   JOIN package
     ON package_owner.package_id = package.id
   WHERE package.name = $1"
  |> pgo.execute(db, [pgo.text(package.name)], dynamic.element(0, dynamic.int))
  |> result.map(fn(r) { r.rows })
}

fn add_new_package_owners(
  db: pgo.Connection,
  owners: List(HexUser),
  current_owners: List(Int),
  package: hexpm.Package,
) {
  owners
  |> list.filter(fn(o) { list.contains(current_owners, o.id) })
  |> list.map(fn(u) {
    let hex_user_id = pgo.int(u.id)
    let name = pgo.text(package.name)
    "INSERT INTO package_owner (hex_user_id, package_id)
       SELECT $1, package.id FROM package WHERE package.name = $2"
    |> pgo.execute(db, [hex_user_id, name], dynamic.dynamic)
  })
  |> result.all()
}

fn remove_old_package_owners(
  db: pgo.Connection,
  owners: List(HexUser),
  current_owners: List(Int),
  package: hexpm.Package,
) {
  current_owners
  |> list.filter(fn(id) {
    owners
    |> list.map(fn(o) { o.id })
    |> list.contains(id)
    |> bool.negate()
  })
  |> list.map(fn(u) {
    let hex_user_id = pgo.int(u)
    let name = pgo.text(package.name)
    "DELETE FROM package_owner
     USING package
     WHERE package_owner.hex_user_id = $1
       AND package_owner.package_id = package.id
       AND package.name = $2"
    |> pgo.execute(db, [hex_user_id, name], dynamic.dynamic)
  })
  |> result.all()
}

pub fn sync_package_owners(db: pgo.Connection, package: hexpm.Package) {
  case package.owners {
    None -> Ok(Nil)
    Some(owners) -> {
      use news <- result.try(upsert_package_owners(db, owners))
      use curr <- result.try(get_current_package_owners(db, package))
      use _ <- result.try(add_new_package_owners(db, news, curr, package))
      use _ <- result.try(remove_old_package_owners(db, news, curr, package))
      Ok(Nil)
    }
  }
  |> result.map_error(error.DatabaseError)
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
