import data/analytics
import data/ir
import data/package
import data/type_search
import decrypt
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/hexpm
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/package_interface
import gleam/pair
import gleam/result
import gleam/string
import gleam/time/calendar
import gleam/time/timestamp.{type Timestamp}
import helpers
import jupiter/data/hex_read
import jupiter/data/hex_user.{type HexUser}
import jupiter/data/interfaces.{type Interfaces, Interfaces}
import jupiter/gleam/context
import jupiter/loss.{type Loss}
import pog

pub type SignatureKind {
  TypeAlias
  TypeDefinition
  Constant
  Function
}

pub fn get_last_hex_date(db: pog.Connection) {
  "SELECT id::text, last_check FROM hex_read ORDER BY last_check DESC LIMIT 1"
  |> pog.query
  |> pog.returning(hex_read.decoder())
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(response) {
    response.rows
    |> list.first
    |> result.map(fn(r) { r.last_check })
    |> result.lazy_unwrap(fn() { timestamp.from_unix_seconds(0) })
  })
}

pub fn upsert_most_recent_hex_timestamp(db: pog.Connection, latest: Timestamp) {
  "INSERT INTO hex_read (id, last_check)
   VALUES ('11111111-1111-8111-8111-111111111111', $1)
   ON CONFLICT (id) DO UPDATE
     SET last_check = $1
   RETURNING id::text, last_check"
  |> pog.query
  |> pog.parameter(pog.timestamp(latest))
  |> pog.returning(hex_read.decoder())
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.try(fn(response) {
    let err = "Upsert most recent hex timestamp failed"
    response.rows
    |> list.first
    |> result.replace_error(loss.CustomError(err))
  })
}

pub fn upsert_search_analytics(db: pog.Connection, query: String) {
  "INSERT INTO search_analytics (query)
   VALUES ($1)
   ON CONFLICT (query) DO UPDATE
     SET occurences = search_analytics.occurences + 1
   RETURNING *"
  |> pog.query
  |> pog.parameter(pog.text(query))
  |> pog.returning(decode.dynamic)
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.try(fn(response) {
    let err = "Upsert search analytics failed"
    response.rows
    |> list.first()
    |> result.replace_error(loss.CustomError(err))
  })
}

pub fn select_more_popular_packages(db: pog.Connection) {
  use ranked <- result.try({
    "SELECT name, repository, rank, (popularity -> 'github')::int AS popularity
    FROM package
    WHERE repository IS NOT NULL
    ORDER BY rank DESC
    LIMIT 22"
    |> pog.query
    |> pog.returning(analytics.package_decoder())
    |> pog.execute(db)
    |> result.map(fn(r) { r.rows })
    |> result.map_error(loss.DatabaseError)
  })
  use popular <- result.try({
    "SELECT name, repository, rank, (popularity -> 'github')::int AS popularity
    FROM package
    WHERE popularity -> 'github' IS NOT NULL
      AND repository IS NOT NULL
      AND name != 'funtil'
      AND name != 'dew'
    ORDER BY popularity -> 'github' DESC
    LIMIT 23"
    |> pog.query
    |> pog.returning(analytics.package_decoder())
    |> pog.execute(db)
    |> result.map(fn(r) { r.rows })
    |> result.map_error(loss.DatabaseError)
  })
  Ok(#(ranked, popular))
}

fn start_day() {
  let now = timestamp.system_time()
  let #(date, _) = timestamp.to_calendar(now, calendar.utc_offset)
  let time = calendar.TimeOfDay(0, 0, 0, 0)
  timestamp.from_calendar(date, time, calendar.utc_offset)
}

pub fn select_last_day_search_analytics(db: pog.Connection) {
  let now = start_day()
  "SELECT query, occurences
   FROM search_analytics
   WHERE updated_at >= $1"
  |> pog.query
  |> pog.parameter(pog.timestamp(now))
  |> pog.returning({
    use query <- decode.field("query", decode.string)
    use occurences <- decode.field("occurences", decode.int)
    decode.success(#(query, occurences))
  })
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(loss.DatabaseError)
}

pub fn upsert_search_analytics_timeseries(
  db: pog.Connection,
  analytic: #(String, Int),
) -> Loss(pog.Returned(Nil)) {
  let now = start_day()
  let #(query, occurences) = analytic
  "INSERT INTO analytics_timeseries (query, occurences, date)
   VALUES ($1, $2, $3)
   ON CONFLICT (query, date) DO UPDATE
     SET occurences = $2"
  |> pog.query
  |> pog.parameter(pog.text(query))
  |> pog.parameter(pog.int(occurences))
  |> pog.parameter(pog.timestamp(now))
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
}

pub fn get_timeseries_count(db: pog.Connection) {
  "SELECT
    SUM(at.occurences - COALESCE(
      (SELECT att.occurences
        FROM analytics_timeseries att
        WHERE att.date < at.date
          AND att.query = at.query
        ORDER BY date DESC
        LIMIT 1),
      0)
    ) searches,
    at.date date
  FROM analytics_timeseries at
  WHERE at.date >= now() - INTERVAL '30 day'
  GROUP BY at.date
  ORDER BY date DESC"
  |> pog.query
  |> pog.returning({
    use searches <- decode.field("searches", decode.int)
    use date <- decode.field("date", decrypt.timestamp())
    decode.success(#(searches, date))
  })
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn upsert_hex_user(db: pog.Connection, owner: hexpm.PackageOwner) {
  "INSERT INTO hex_user (username, email, url)
   VALUES ($1, $2, $3)
   ON CONFLICT (username) DO UPDATE
     SET email = $2, url = $3
   RETURNING id::text, username, email, url, created_at, updated_at"
  |> pog.query
  |> pog.parameter(pog.text(owner.username))
  |> pog.parameter(pog.nullable(pog.text, owner.email))
  |> pog.parameter(pog.text(owner.url))
  |> pog.returning(hex_user.decoder())
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(loss.DatabaseError)
}

fn upsert_package_owners(db: pog.Connection, owners: List(hexpm.PackageOwner)) {
  owners
  |> list.map(upsert_hex_user(db, _))
  |> result.all
  |> result.map(list.flatten)
}

fn get_current_package_owners(db: pog.Connection, package_id: String) {
  "SELECT package_owner.hex_user_id::text AS user_id
   FROM package_owner
   WHERE package_owner.package_id = $1"
  |> pog.query
  |> pog.parameter(pog.text(package_id))
  |> pog.returning(decode.at(["user_id"], decode.string))
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(loss.DatabaseError)
}

pub fn get_total_searches(db: pog.Connection) {
  "SELECT SUM(occurences) occurences FROM search_analytics"
  |> pog.query
  |> pog.returning(decode.at(["occurences"], decode.int))
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(loss.DatabaseError)
}

pub fn get_total_signatures(db: pog.Connection) {
  "SELECT COUNT(*) c FROM package_type_fun_signature"
  |> pog.query
  |> pog.returning(decode.at(["c"], decode.int))
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(loss.DatabaseError)
}

pub fn get_total_packages(db: pog.Connection) {
  "SELECT COUNT(*) c FROM package"
  |> pog.query
  |> pog.returning(decode.at(["c"], decode.int))
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(loss.DatabaseError)
}

fn add_new_package_owners(
  db: pog.Connection,
  owners: List(HexUser),
  current_owners: List(String),
  package_id: String,
) -> Loss(List(pog.Returned(Dynamic))) {
  owners
  |> list.filter(fn(o) { bool.negate(list.contains(current_owners, o.id)) })
  |> list.map(fn(u) {
    "INSERT INTO package_owner (hex_user_id, package_id)
     VALUES ($1, $2)"
    |> pog.query
    |> pog.parameter(pog.text(u.id))
    |> pog.parameter(pog.text(package_id))
    |> pog.returning(decode.dynamic)
    |> pog.execute(db)
  })
  |> result.all()
  |> result.map_error(loss.DatabaseError)
}

fn remove_old_package_owners(
  db: pog.Connection,
  owners: List(HexUser),
  current_owners: List(String),
  package_id: String,
) -> Loss(List(pog.Returned(Nil))) {
  let curr = list.map(owners, fn(o) { o.id })
  current_owners
  |> list.filter(fn(id) { list.contains(curr, id) })
  |> list.map(fn(u) {
    "DELETE FROM package_owner
     WHERE package_owner.hex_user_id = $1
       AND package_owner.package_id = $2"
    |> pog.query
    |> pog.parameter(pog.text(u))
    |> pog.parameter(pog.text(package_id))
    |> pog.execute(db)
  })
  |> result.all()
  |> result.map_error(loss.DatabaseError)
}

pub fn sync_package_owners(
  db: pog.Connection,
  package_id: String,
  owners: List(hexpm.PackageOwner),
) -> Loss(Nil) {
  use news <- result.try(upsert_package_owners(db, owners))
  use curr <- result.try(get_current_package_owners(db, package_id))
  use _ <- result.try(add_new_package_owners(db, news, curr, package_id))
  use _ <- result.try(remove_old_package_owners(db, news, curr, package_id))
  Ok(Nil)
}

pub fn upsert_package(db: pog.Connection, package: hexpm.Package) {
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
   RETURNING id::text"
  |> pog.query
  |> pog.parameter(pog.text(package.name))
  |> pog.parameter(pog.nullable(pog.text, package.html_url))
  |> pog.parameter(pog.nullable(pog.text, package.docs_html_url))
  |> pog.parameter({
    package.meta.links
    |> dict.get("Repository")
    |> option.from_result()
    |> pog.nullable(pog.text, _)
  })
  |> pog.parameter(package.meta.links |> helpers.json_dict() |> pog.text())
  |> pog.parameter(package.meta.licenses |> helpers.json_list() |> pog.text())
  |> pog.parameter(pog.nullable(pog.text, package.meta.description))
  |> pog.returning(decode.at(["id"], decode.string))
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.try(fn(response) {
    response.rows
    |> list.first()
    |> result.replace_error(loss.CustomError("Upsert package error"))
  })
}

pub fn upsert_release(
  db: pog.Connection,
  package_id: String,
  release: hexpm.Release,
  package_interface: Option(String),
  gleam_toml: Option(String),
) -> Loss(pog.Returned(Interfaces)) {
  "INSERT INTO package_release (
     package_id,
     version,
     url,
     package_interface,
     gleam_toml,
     inserted_at
   ) VALUES ($1, $2, $3, $4, $5, $6)
   ON CONFLICT (package_id, version) DO UPDATE
     SET
       url = $3,
       package_interface = $4,
       gleam_toml = $5,
       inserted_at = $6
   RETURNING id::text, package_interface, gleam_toml"
  |> pog.query
  |> pog.parameter(pog.text(package_id))
  |> pog.parameter(pog.text(release.version))
  |> pog.parameter(pog.text(release.url))
  |> pog.parameter(pog.nullable(pog.text, package_interface))
  |> pog.parameter(pog.nullable(pog.text, gleam_toml))
  |> pog.parameter(pog.timestamp(release.inserted_at))
  |> pog.returning({
    use id <- decode.field("id", decode.string)
    use it <- decode.field("package_interface", decode.optional(decode.string))
    use gl <- decode.field("gleam_toml", decode.optional(decode.string))
    decode.success(Interfaces(id, it, gl))
  })
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
}

pub fn lookup_release(
  db: pog.Connection,
  package_id: String,
  release: hexpm.Release,
) -> Loss(Interfaces) {
  "SELECT id::text, package_interface, gleam_toml
  FROM package_release
  WHERE package_id = $1 AND version = $2"
  |> pog.query
  |> pog.parameter(pog.text(package_id))
  |> pog.parameter(pog.text(release.version))
  |> pog.returning({
    use id <- decode.field("id", decode.string)
    use it <- decode.field("package_interface", decode.optional(decode.string))
    use gl <- decode.field("gleam_toml", decode.optional(decode.string))
    decode.success(Interfaces(id, it, gl))
  })
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.try(fn(res) {
    res.rows
    |> list.first()
    |> result.replace_error(loss.CustomError(""))
  })
}

pub fn add_package_gleam_constraint(
  db: pog.Connection,
  constraint: String,
  release_id: String,
) -> Loss(Nil) {
  "UPDATE package_release SET gleam_constraint = $1 WHERE id = $2"
  |> pog.query
  |> pog.parameter(pog.text(constraint))
  |> pog.parameter(pog.text(release_id))
  |> pog.execute(db)
  |> result.replace(Nil)
  |> result.map_error(loss.DatabaseError)
}

pub fn add_package_retirement(
  db: pog.Connection,
  retirement: hexpm.ReleaseRetirement,
  release_id: String,
) -> Loss(Nil) {
  "UPDATE package_release SET retirement = $1 WHERE id = $2"
  |> pog.query
  |> pog.parameter(pog.text(encode_retirement(retirement)))
  |> pog.parameter(pog.text(release_id))
  |> pog.execute(db)
  |> result.replace(Nil)
  |> result.map_error(loss.DatabaseError)
}

fn encode_retirement(retirement: hexpm.ReleaseRetirement) {
  json.object([
    #("message", json.nullable(retirement.message, json.string)),
    #("reason", {
      json.string(case retirement.reason {
        hexpm.OtherReason -> "other-reason"
        hexpm.Invalid -> "invalid"
        hexpm.Security -> "security"
        hexpm.Deprecated -> "deprecated"
        hexpm.Renamed -> "renamed"
      })
    }),
  ])
  |> json.to_string()
}

pub fn get_package_release_ids(
  db: pog.Connection,
  package: package_interface.Package,
) -> Loss(#(String, String)) {
  "SELECT
    package.id::text package_id,
    package_release.id::text package_release_id
  FROM package
  JOIN package_release
    ON package_release.package_id = package.id
  WHERE package.name = $1
    AND package_release.version = $2"
  |> pog.query
  |> pog.parameter(pog.text(package.name))
  |> pog.parameter(pog.text(package.version))
  |> pog.returning({
    use package_id <- decode.field("package_id", decode.string)
    use package_release_id <- decode.field("package_release_id", decode.string)
    decode.success(#(package_id, package_release_id))
  })
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.try(fn(response) {
    list.first(response.rows)
    |> result.replace_error(loss.CustomError(
      "No release found for " <> package.name <> "@" <> package.version,
    ))
  })
}

pub fn upsert_package_module(db: pog.Connection, module: context.Module) {
  use response <- result.try({
    "INSERT INTO package_module (name, documentation, package_release_id)
     VALUES ($1, $2, $3)
     ON CONFLICT (name, package_release_id) DO UPDATE
       SET documentation = $2
     RETURNING id::text"
    |> pog.query
    |> pog.parameter(pog.text(module.name))
    |> pog.parameter({
      module.module.documentation
      |> string.join("\n")
      |> pog.text()
    })
    |> pog.parameter(pog.text(module.release_id))
    |> pog.returning(decode.at(["id"], decode.string))
    |> pog.execute(db)
    |> result.map_error(loss.DatabaseError)
  })
  response.rows
  |> list.first()
  |> result.replace_error(loss.CustomError(
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
  db db: pog.Connection,
  kind kind: SignatureKind,
  name name: String,
  documentation documentation: Option(String),
  metadata metadata: json.Json,
  signature signature: String,
  json_signature json_signature: json.Json,
  signature_ir signature_ir: json.Json,
  arity arity: Int,
  type_names type_names: List(String),
  module_id module_id: String,
  deprecation deprecation: Option(package_interface.Deprecation),
  implementations implementations: Option(package_interface.Implementations),
) -> Loss(List(String)) {
  "INSERT INTO package_type_fun_signature (
     name,
     documentation,
     signature_,
     signature_json,
     kind,
     metadata,
     package_module_id,
     deprecation,
     implementations,
     signature_ir,
     arity,
     type_names,
     type_names_key
   ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
   ON CONFLICT (package_module_id, name) DO UPDATE
     SET
       documentation = $2,
       signature_ = $3,
       signature_json = $4,
       kind = $5,
       metadata = $6,
       deprecation = $8,
       implementations = $9,
       signature_ir = $10,
       arity = $11,
       type_names = $12,
       type_names_key = $13
   RETURNING id::text"
  |> pog.query
  |> pog.parameter(pog.text(name))
  |> pog.parameter({
    documentation
    |> option.unwrap("")
    |> string.trim
    |> pog.text
  })
  |> pog.parameter(pog.text(signature))
  |> pog.parameter(json_signature |> json.to_string() |> pog.text())
  |> pog.parameter(pog.text(kind_to_string(kind)))
  |> pog.parameter(metadata |> json.to_string() |> pog.text())
  |> pog.parameter(pog.text(module_id))
  |> pog.parameter({
    deprecation
    |> option.map(fn(d) { d.message })
    |> pog.nullable(pog.text, _)
  })
  |> pog.parameter({
    implementations
    |> option.map(implementations_pgo)
    |> option.map(pog.text)
    |> option.unwrap(pog.null())
  })
  |> pog.parameter(signature_ir |> json.to_string() |> pog.text())
  |> pog.parameter(pog.int(arity))
  |> pog.parameter(pog.array(pog.text, type_names))
  |> pog.parameter(pog.text(type_names_key(type_names)))
  |> pog.returning(decode.at(["id"], decode.string))
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

fn kind_to_string(kind: SignatureKind) -> String {
  case kind {
    Function -> "function"
    TypeAlias -> "type_alias"
    TypeDefinition -> "type_definition"
    Constant -> "constant"
  }
}

pub fn find_similar_type_names(db: pog.Connection, name: String) {
  "SELECT *
   FROM (VALUES
     ('Int'),
     ('Float'),
     ('String'),
     ('Bool'),
     ('List'),
     ('Nil'),
     ('Result'),
     ('BitArray')
   ) AS t (name)
   WHERE levenshtein_less_equal(name, $1, 2) <= 2
   UNION
   SELECT DISTINCT ON (name) name
   FROM package_type_fun_signature
   WHERE (kind = 'type_definition' OR kind = 'type_alias')
     AND levenshtein_less_equal(name, $1, 2) <= 2"
  |> pog.query
  |> pog.parameter(pog.text(name))
  |> pog.returning(decode.at(["name"], decode.string))
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

/// Every text tier skips the recursive-type placeholders whose second upsert
/// never landed (`signature_ = ''`, JSON `null` in `signature_json`/`metadata`,
/// 123 rows over 24 packages in the 2026-07 snapshot): `type_search.decoder()`
/// cannot decode them, and one such row fails the whole batch — a silent empty
/// tier. The root cause is in ingestion; see `signature-ir-todos.md`.
pub fn name_search(db: pog.Connection, query: String) {
  "SELECT DISTINCT ON (package_rank, ordering, package_name, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.signature_json AS json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     r.version_ordering AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   WHERE s.name = $1
     AND s.signature_ <> ''
     AND r.version SIMILAR TO '[0-9]*.[0-9]*.[0-9]*'
   ORDER BY package_rank DESC, ordering DESC, package_name, type_name, signature_kind, module_name
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.text(query))
  |> pog.returning(type_search.decoder())
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn module_and_name_search(db: pog.Connection, query: String) {
  "WITH splitted_name AS (SELECT string_to_array($1, '.') AS full_name)
   SELECT DISTINCT ON (package_rank, ordering, package_name, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.signature_json AS json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     r.version_ordering AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   JOIN splitted_name s_n
     ON true
   WHERE s.name = s_n.full_name[2]
   AND m.name LIKE '%' || lower(s_n.full_name[1]) || '%'
     AND s.signature_ <> ''
     AND r.version SIMILAR TO '[0-9]*.[0-9]*.[0-9]*'
   ORDER BY package_rank DESC, ordering DESC, package_name, type_name, signature_kind, module_name
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.text(query))
  |> pog.returning(type_search.decoder())
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

fn transform_query(q: String) {
  q
  |> string.replace("(", " ")
  |> string.replace(")", " ")
  |> string.replace(",", " ")
  |> string.replace(".", " ")
  |> string.split(" ")
  |> list.filter(fn(item) { !string.is_empty(item) })
  |> string.join(" ")
}

pub fn content_search(db: pog.Connection, query: String) {
  "SELECT DISTINCT ON (package_rank, ordering, package_name, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.signature_json AS json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     r.version_ordering AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   WHERE s.name <> $1
     AND (
      s.name ILIKE '%' || $1 || '%'
      OR s.signature_compact ILIKE '%' || replace($2, ' ', '%') || '%'
    )
     AND s.signature_ <> ''
     AND r.version SIMILAR TO '[0-9]*.[0-9]*.[0-9]*'
   ORDER BY package_rank DESC, ordering DESC, package_name, type_name, signature_kind, module_name
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.text(query))
  |> pog.parameter(pog.text(transform_query(query)))
  |> pog.returning(type_search.decoder())
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn signature_search(db: pog.Connection, q: String) {
  "SELECT DISTINCT ON (package_rank, ordering, package_name, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.signature_json AS json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     r.version_ordering AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   WHERE to_tsvector('english', s.signature_) @@ websearch_to_tsquery($1)
     AND s.signature_ <> ''
     AND r.version SIMILAR TO '[0-9]*.[0-9]*.[0-9]*'
   ORDER BY package_rank DESC, ordering DESC, package_name, type_name, signature_kind, module_name
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.text(q))
  |> pog.returning(type_search.decoder())
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn documentation_search(db: pog.Connection, q: String) {
  "SELECT DISTINCT ON (package_rank, ordering, package_name, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.signature_json AS json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     r.version_ordering AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   WHERE to_tsvector('english', s.documentation) @@ websearch_to_tsquery($1)
     AND s.signature_ <> ''
     AND r.version SIMILAR TO '[0-9]*.[0-9]*.[0-9]*'
   ORDER BY package_rank DESC, ordering DESC, package_name, type_name, signature_kind, module_name
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.text(q))
  |> pog.returning(type_search.decoder())
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

/// Search modules whose name matches `q`, returning every matching signature
/// across *all* releases of the package. Narrowing down to the latest version
/// is intentionally NOT done here: the backend exposes every version, and the
/// frontend keeps only the most recent one per package (with a "Show old
/// versions" toggle to reveal the rest). See `venus/data/model` filtering.
///
/// The `SIMILAR TO` guard on `r.version` only discards non-semver releases
/// (pre-releases, build tags, etc.), so downstream code can always parse and
/// order the version.
pub fn module_search(db: pog.Connection, q: String) {
  "SELECT DISTINCT ON (package_rank, ordering, package_name, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.signature_json AS json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     r.version_ordering AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   WHERE m.name LIKE '%' || lower($1) || '%'
     -- Keep only well-formed semver releases (e.g. '1.2.3'); pre-releases such
     -- as '1.2.3-rc1' are excluded. Picking the latest version is the
     -- frontend's job, not this query's.
     AND s.signature_ <> ''
     AND r.version SIMILAR TO '[0-9]*.[0-9]*.[0-9]*'
   ORDER BY package_rank DESC, ordering DESC, package_name, type_name, signature_kind, module_name"
  |> pog.query
  |> pog.parameter(pog.text(q))
  |> pog.returning(type_search.decoder())
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

/// Phase 1 of type search: narrow the whole signature table to at most 100
/// candidates by shape, in both directions of generality, then let phase 2
/// (`data/ir/unify`) verify each one structurally. Two indexed predicates, OR-ed:
///
/// - `type_names @> $2` (composite GIN on `(arity, type_names)`): the candidate
///   mentions every query name — exact matches and candidates *more specific*
///   than the query (the query's holes and variables absorb the extra names).
/// - `type_names_key = ANY($3)` (btree on `(arity, type_names_key)`): the
///   candidate's names are a *subset* of the query's — candidates *more general*
///   than the query (`fn(List(a)) -> Bool` for `(List(Int)) -> Bool`). The caller
///   enumerates the proper subsets of the query's names as canonical keys
///   (sorted names joined by `,`); `<@` on the GIN was measured far too slow
///   (it rechecks every row sharing a name), equality on a text key is a
///   handful of btree probes the GIN cannot claim.
///
/// One row per declaration, not per release: `DISTINCT ON (package, module,
/// name, kind)` keeps the latest version (`package_release.version_ordering`,
/// the semver array generated by Postgres), as the old trie tier did. Then ordered closest-first by
/// `abs(cardinality(type_names) - n)` — the number of names one side has to
/// absorb, whichever side is the general one — with package rank breaking ties.
/// Rows with a null IR are excluded (nothing to verify against), and so are
/// the recursive-type placeholders whose second upsert never landed
/// (`signature_ = ''`, JSON `null` in `signature_json`/`metadata`): they carry
/// an IR since the backfill, but `type_search.decoder()` cannot decode them and
/// one such row fails the whole batch.
///
/// Two shape guards, both measured: `type_definition` rows are skipped (phase 2
/// never verifies one — its constructors are found by name — and the 22,771
/// parameterless enums all carry the empty key, 1.4 s → 0.4 s on `List(Int)`);
/// and a query with no names at all (`fn(a) -> a`) does not run `@>` (`@> '{}'`
/// is true for the whole arity, 3 s) but only the empty key, the fully generic
/// candidates (112 ms) — the only ones the distance ordering would keep anyway.
///
/// What this still misses: a candidate more general than the query whose names
/// appear only under a query hole (`fn(List(a)) -> b` for `(_) -> Bool`). Sound
/// phase-1 selection for that direction with holes is `arity` alone; deferred.
pub fn fingerprint_candidates(
  db db: pog.Connection,
  arity arity: Int,
  type_names type_names: List(String),
  subset_keys subset_keys: List(String),
) -> Loss(List(#(type_search.TypeSearch, ir.Declaration))) {
  "SELECT * FROM (
     SELECT DISTINCT ON (p.name, m.name, s.name, s.kind)
       s.name type_name,
       s.documentation documentation,
       s.kind signature_kind,
       s.metadata metadata,
       s.signature_json AS json_signature,
       s.signature_ir AS signature_ir,
       m.name module_name,
       p.name package_name,
       r.version version,
       p.rank package_rank,
       abs(cardinality(s.type_names) - $4) distance
     FROM package_type_fun_signature s
     JOIN package_module m
       ON m.id = s.package_module_id
     JOIN package_release r
       ON m.package_release_id = r.id
     JOIN package p
       ON p.id = r.package_id
     WHERE s.arity = $1
       AND s.kind <> 'type_definition'
       AND ((cardinality($2::text[]) > 0 AND s.type_names @> $2::text[])
         OR s.type_names_key = ANY($3::text[]))
       AND s.signature_ir IS NOT NULL
       AND s.signature_ <> ''
       AND r.version SIMILAR TO '[0-9]*.[0-9]*.[0-9]*'
     ORDER BY p.name, m.name, s.name, s.kind, r.version_ordering DESC
   ) candidates
   ORDER BY distance, package_rank DESC
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.int(arity))
  |> pog.parameter(pog.array(pog.text, type_names))
  |> pog.parameter(pog.array(pog.text, subset_keys))
  |> pog.parameter(pog.int(list.length(type_names)))
  |> pog.returning(candidate_decoder())
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

/// The canonical key of a fingerprint's names: sorted (as `ir.fingerprint`
/// emits them), joined by `,`. Stored in `type_names_key` next to the array so
/// the subset lookup is a btree equality the GIN cannot claim.
pub fn type_names_key(type_names: List(String)) -> String {
  string.join(type_names, ",")
}

/// Beyond this many names the subset enumeration (2^n keys) stops paying for
/// itself; such queries only get the `@>` direction.
const max_subset_names = 6

/// The canonical keys of every *proper* subset of the query's names, the empty
/// set included (`fn(a) -> b` is a subset of everything) and the full set
/// excluded (`@>` already covers it) — except for a nameless query, whose only
/// subset is the empty key itself, since `@>` is switched off for it.
/// `type_names` arrives sorted from `ir.fingerprint_type`, so each subset is
/// emitted in canonical order.
pub fn subset_keys(type_names: List(String)) -> List(String) {
  use <- bool.guard(when: list.is_empty(type_names), return: [""])
  use <- bool.guard(
    when: list.length(type_names) > max_subset_names,
    return: [],
  )
  type_names
  |> list.fold([[]], fn(subsets, name) {
    list.append(subsets, list.map(subsets, fn(s) { list.append(s, [name]) }))
  })
  |> list.filter(fn(s) { list.length(s) < list.length(type_names) })
  |> list.map(type_names_key)
}

/// A phase-1 row: the display record plus the candidate's structural IR, decoded
/// from `signature_ir` for phase-2 verification.
fn candidate_decoder() {
  use item <- decode.then(type_search.decoder())
  use declaration <- decode.field("signature_ir", decrypt.json(ir.decoder()))
  decode.success(#(item, declaration))
}

pub fn select_gleam_toml(db: pog.Connection, offset: Int) {
  "SELECT gleam_toml
   FROM package_release
   WHERE gleam_toml IS NOT NULL
   ORDER BY id
   LIMIT 100
   OFFSET $1"
  |> pog.query
  |> pog.parameter(pog.int(offset))
  |> pog.returning(decode.at(["gleam_toml"], decode.string))
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn update_package_rank(db: pog.Connection, package: String, rank: Int) {
  "UPDATE package SET rank = $2 WHERE name = $1"
  |> pog.query
  |> pog.parameter(pog.text(package))
  |> pog.parameter(pog.int(rank))
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
}

pub fn select_package_repository_address(db: pog.Connection, offset: Int) {
  "SELECT id::text, repository FROM package LIMIT 100 OFFSET $1"
  |> pog.query
  |> pog.parameter(pog.int(offset))
  |> pog.returning({
    use id <- decode.field("id", decode.string)
    use repo <- decode.field("repository", decode.optional(decode.string))
    decode.success({
      case repo {
        Some(repo) -> Some(#(id, repo))
        None -> None
      }
    })
  })
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn update_package_popularity(
  db: pog.Connection,
  url: String,
  popularity: Dict(String, Int),
) -> Loss(pog.Returned(Nil)) {
  "UPDATE package SET popularity = $2 WHERE repository = $1"
  |> pog.query
  |> pog.parameter(pog.text(url))
  |> pog.parameter({
    dict.to_list(popularity)
    |> list.map(pair.map_second(_, json.int))
    |> json.object
    |> json.to_string
    |> pog.text
  })
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
}

pub fn select_package_by_popularity(db: pog.Connection, page: Int) {
  let offset = 40 * page
  "SELECT
    name,
    repository,
    documentation,
    hex_url,
    licenses,
    description,
    rank,
    popularity
  FROM package
  WHERE popularity -> 'github' IS NOT NULL
  ORDER BY popularity -> 'github' DESC
  LIMIT 40
  OFFSET $1"
  |> pog.query
  |> pog.parameter(pog.int(offset))
  |> pog.returning(package.decoder())
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(loss.DatabaseError)
}

pub fn select_package_by_updated_at(db: pog.Connection) {
  "SELECT
    name,
    repository,
    documentation,
    hex_url,
    licenses,
    description,
    rank,
    popularity
  FROM package
  ORDER BY updated_at DESC"
  |> pog.query
  |> pog.returning(package.decoder())
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(loss.DatabaseError)
}

pub fn insert_analytics(
  db: pog.Connection,
  id: String,
  table_name: String,
  content: Dict(String, Int),
) -> Loss(pog.Returned(Nil)) {
  "INSERT INTO analytics (foreign_id, table_name, content, day)
   VALUES ($1, $2, $3, $4)
   ON CONFLICT (foreign_id, table_name, day) DO UPDATE
     SET content = $3"
  |> pog.query
  |> pog.parameter(pog.text(id))
  |> pog.parameter(pog.text(table_name))
  |> pog.parameter({
    content
    |> dict.to_list
    |> list.map(pair.map_second(_, json.int))
    |> json.object
    |> json.to_string
    |> pog.text
  })
  |> pog.parameter(pog.timestamp(start_day()))
  |> pog.execute(db)
  |> result.map_error(loss.DatabaseError)
}
