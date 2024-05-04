import backend/data/hex_read.{type HexRead, HexRead}
import backend/data/hex_user.{type HexUser}
import backend/error
import backend/gleam/context
import birl.{type Time}
import gleam/bool
import gleam/dict
import gleam/dynamic
import gleam/hexpm
import gleam/io
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/package_interface
import gleam/pgo
import gleam/result
import gleam/string
import helpers

pub type SignatureNature {
  TypeAlias
  TypeDefinition
  Constant
  Function
}

pub fn get_last_hex_date(db: pgo.Connection) {
  "SELECT id, last_check FROM hex_read ORDER BY last_check DESC LIMIT 1"
  |> pgo.execute(db, [], hex_read.decode)
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
  |> pgo.execute(db, [timestamp], hex_read.decode)
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
  |> pgo.execute(db, [username, email, url], hex_user.decode)
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
  let hex_url = pgo.nullable(pgo.text, package.html_url)
  let docs = pgo.nullable(pgo.text, package.docs_html_url)
  let repo =
    package.meta.links
    |> dict.get("Repository")
    |> option.from_result()
    |> pgo.nullable(pgo.text, _)
  let links =
    package.meta.links
    |> helpers.json_dict()
    |> pgo.text()
  let licenses =
    package.meta.licenses
    |> helpers.json_list()
    |> pgo.text()
  let description = pgo.nullable(pgo.text, package.meta.description)
  "INSERT INTO package
    (name, repository, documentation, hex_url, links, licenses, description)
   VALUES ($1, $2, $3, $4, $5, $6, $7)
   ON CONFLICT (name) DO UPDATE
     SET
       repository = $2,
       documentation = $3,
       hex_url = $4,
       links = $5,
       licenses = $6,
       description = $7
   RETURNING id"
  |> pgo.execute(
    db,
    [name, repo, docs, hex_url, links, licenses, description],
    dynamic.element(0, dynamic.int),
  )
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

pub fn add_package_gleam_constraint(
  db: pgo.Connection,
  constraint: String,
  release_id: Int,
) {
  let constraint = pgo.text(constraint)
  let release_id = pgo.int(release_id)
  "UPDATE package_release SET gleam_constraint = $1 WHERE id = $2"
  |> pgo.execute(db, [constraint, release_id], dynamic.dynamic)
  |> result.replace(Nil)
  |> result.map_error(error.DatabaseError)
}

pub fn get_package_release_ids(
  db: pgo.Connection,
  package: package_interface.Package,
) {
  use response <- result.try({
    let args = [pgo.text(package.name), pgo.text(package.version)]
    "SELECT
       package.id package_id,
       package_release.id package_release_id
     FROM package
     JOIN package_release
       ON package_release.package_id = package.id
     WHERE package.name = $1
       AND package_release.version = $2"
    |> pgo.execute(db, args, dynamic.tuple2(dynamic.int, dynamic.int))
    |> result.map_error(error.DatabaseError)
  })
  response.rows
  |> list.first()
  |> result.replace_error(error.UnknownError(
    "No release found for " <> package.name <> "@" <> package.version,
  ))
}

pub fn upsert_package_module(db: pgo.Connection, module: context.Module) {
  use response <- result.try({
    let args = [
      pgo.text(module.name),
      module.module.documentation
        |> string.join("\n")
        |> pgo.text(),
      pgo.int(module.release_id),
    ]
    "INSERT INTO package_module (name, documentation, package_release_id)
     VALUES ($1, $2, $3)
     ON CONFLICT (name, package_release_id) DO UPDATE
       SET documentation = $2
     RETURNING id"
    |> pgo.execute(db, args, dynamic.element(0, dynamic.int))
    |> result.map_error(error.DatabaseError)
  })
  response.rows
  |> list.first()
  |> result.replace_error(error.UnknownError(
    "No module found for " <> module.name,
  ))
}

fn implementations_pgo(implementations: package_interface.Implementations) {
  [
    #("gleam", implementations.gleam),
    #("erlang", implementations.uses_erlang_externals),
    #("javascript", implementations.uses_javascript_externals),
  ]
  |> list.filter(fn(t) { t.1 })
  |> list.map(fn(t) { t.0 })
  |> string.join(",")
}

pub fn upsert_package_type_fun_signature(
  db db: pgo.Connection,
  nature nature: SignatureNature,
  name name: String,
  documentation documentation: Option(String),
  metadata metadata: json.Json,
  signature signature: String,
  json_signature json_signature: json.Json,
  parameters parameters: List(Int),
  module_id module_id: Int,
  deprecation deprecation: Option(package_interface.Deprecation),
  implementations implementations: Option(package_interface.Implementations),
) {
  let nature = case nature {
    Function -> "function"
    TypeAlias -> "type_alias"
    TypeDefinition -> "type_definition"
    Constant -> "constant"
  }
  "INSERT INTO package_type_fun_signature (
     name,
     documentation,
     signature_,
     json_signature,
     nature,
     parameters,
     metadata,
     package_module_id,
     deprecation,
     implementations
   ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
   ON CONFLICT (package_module_id, name) DO UPDATE
     SET
       documentation = $2,
       signature_ = $3,
       json_signature = $4,
       nature = $5,
       parameters = $6,
       metadata = $7,
       deprecation = $9,
       implementations = $10"
  |> pgo.execute(
    db,
    [
      pgo.text(name),
      documentation
        |> option.unwrap("")
        |> string.trim()
        |> pgo.text(),
      pgo.text(signature),
      json_signature
        |> json.to_string()
        |> pgo.text(),
      pgo.text(nature),
      dynamic.unsafe_coerce(dynamic.from(parameters)),
      metadata
        |> json.to_string()
        |> pgo.text(),
      pgo.int(module_id),
      deprecation
        |> option.map(fn(d) { d.message })
        |> pgo.nullable(pgo.text, _),
      implementations
        |> option.map(implementations_pgo)
        |> option.map(pgo.text)
        |> option.unwrap(pgo.null()),
    ],
    dynamic.dynamic,
  )
  |> result.map_error(error.DatabaseError)
  |> result.replace(Nil)
}
