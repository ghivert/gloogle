import backend/data/hex_read.{type HexRead, HexRead}
import backend/data/hex_user.{type HexUser}
import backend/error
import backend/gleam/context
import birl.{type Time}
import data/analytics
import data/package
import data/type_search
import gleam/bool
import gleam/dict.{type Dict}
import gleam/dynamic
import gleam/hexpm
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/package_interface
import gleam/pair
import gleam/result
import gleam/string
import helpers
import pog

@external(erlang, "backend_ffi", "coerce")
fn coerce(a: a) -> b

pub type SignatureKind {
  TypeAlias
  TypeDefinition
  Constant
  Function
}

pub fn get_last_hex_date(db: pog.Connection) {
  "SELECT id, last_check FROM hex_read ORDER BY last_check DESC LIMIT 1"
  |> pog.query
  |> pog.returning(hex_read.decode)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(response) {
    response.rows
    |> list.first()
    |> result.unwrap(HexRead(0, birl.from_unix(0)))
    |> fn(h: HexRead) { h.last_check }
  })
}

pub fn upsert_most_recent_hex_timestamp(db: pog.Connection, latest: Time) {
  "INSERT INTO hex_read
   OVERRIDING SYSTEM VALUE
   VALUES (1, $1)
   ON CONFLICT (id) DO UPDATE
     SET last_check = $1
   RETURNING id, last_check"
  |> pog.query
  |> pog.parameter(helpers.convert_time(latest))
  |> pog.returning(hex_read.decode)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(response) {
    let err = "Upsert most recent hex timestamp failed"
    response.rows
    |> list.first()
    |> result.replace_error(error.UnknownError(err))
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
  |> pog.returning(dynamic.dynamic)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(response) {
    let err = "Upsert search analytics failed"
    response.rows
    |> list.first()
    |> result.replace_error(error.UnknownError(err))
  })
}

pub fn select_more_popular_packages(db: pog.Connection) {
  use ranked <- result.try({
    "SELECT name, repository, rank, (popularity -> 'github')::int AS popularity
    FROM package
    ORDER BY rank DESC
    LIMIT 22"
    |> pog.query
    |> pog.returning(analytics.decode_package)
    |> pog.execute(db)
    |> result.map(fn(r) { r.rows })
    |> result.map_error(error.DatabaseError)
  })
  use popular <- result.try({
    "SELECT name, repository, rank, (popularity -> 'github')::int AS popularity
    FROM package
    WHERE popularity -> 'github' IS NOT NULL
      AND name != 'funtil'
      AND name != 'dew'
    ORDER BY popularity -> 'github' DESC
    LIMIT 23"
    |> pog.query
    |> pog.returning(analytics.decode_package)
    |> pog.execute(db)
    |> result.map(fn(r) { r.rows })
    |> result.map_error(error.DatabaseError)
  })
  Ok(#(ranked, popular))
}

pub fn select_last_day_search_analytics(db: pog.Connection) {
  let #(date, _) = birl.to_erlang_universal_datetime(birl.now())
  let now = birl.from_erlang_universal_datetime(#(date, #(0, 0, 0)))
  "SELECT query, occurences
   FROM search_analytics
   WHERE updated_at >= $1"
  |> pog.query
  |> pog.parameter(helpers.convert_time(now))
  |> pog.returning(dynamic.decode2(
    pair.new,
    dynamic.field("query", dynamic.string),
    dynamic.field("occurences", dynamic.int),
  ))
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

pub fn upsert_search_analytics_timeseries(
  db: pog.Connection,
  analytic: #(String, Int),
) {
  let #(date, _) = birl.to_erlang_universal_datetime(birl.now())
  let now = birl.from_erlang_universal_datetime(#(date, #(0, 0, 0)))
  let #(query, occurences) = analytic
  "INSERT INTO analytics_timeseries (query, occurences, date)
   VALUES ($1, $2, $3)
   ON CONFLICT (query, date) DO UPDATE
     SET occurences = $2"
  |> pog.query
  |> pog.parameter(pog.text(query))
  |> pog.parameter(pog.int(occurences))
  |> pog.parameter(helpers.convert_time(now))
  |> pog.returning(dynamic.dynamic)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
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
  |> pog.returning(dynamic.decode2(
    pair.new,
    dynamic.field("searches", dynamic.int),
    dynamic.field("date", helpers.decode_time),
  ))
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn upsert_hex_user(db: pog.Connection, owner: hexpm.PackageOwner) {
  "INSERT INTO hex_user (username, email, url)
   VALUES ($1, $2, $3)
   ON CONFLICT (username) DO UPDATE
     SET email = $2, url = $3
   RETURNING id, username, email, url, created_at, updated_at"
  |> pog.query
  |> pog.parameter(pog.text(owner.username))
  |> pog.parameter(pog.nullable(pog.text, owner.email))
  |> pog.parameter(pog.text(owner.url))
  |> pog.returning(hex_user.decode)
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

fn upsert_package_owners(db: pog.Connection, owners: List(hexpm.PackageOwner)) {
  owners
  |> list.map(upsert_hex_user(db, _))
  |> result.all()
  |> result.map(list.flatten)
}

fn get_current_package_owners(db: pog.Connection, package_id: Int) {
  "SELECT package_owner.hex_user_id AS user_id
   FROM package_owner
   WHERE package_owner.package_id = $1"
  |> pog.query
  |> pog.parameter(pog.int(package_id))
  |> pog.returning(dynamic.field("user_id", dynamic.int))
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

pub fn get_total_searches(db: pog.Connection) {
  "SELECT SUM(occurences) occurences FROM search_analytics"
  |> pog.query
  |> pog.returning(dynamic.field("occurences", dynamic.int))
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

pub fn get_total_signatures(db: pog.Connection) {
  "SELECT COUNT(*) c FROM package_type_fun_signature"
  |> pog.query
  |> pog.returning(dynamic.field("c", dynamic.int))
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

pub fn get_total_packages(db: pog.Connection) {
  "SELECT COUNT(*) c FROM package"
  |> pog.query
  |> pog.returning(dynamic.field("c", dynamic.int))
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

fn add_new_package_owners(
  db: pog.Connection,
  owners: List(HexUser),
  current_owners: List(Int),
  package_id: Int,
) {
  owners
  |> list.filter(fn(o) { bool.negate(list.contains(current_owners, o.id)) })
  |> list.map(fn(u) {
    "INSERT INTO package_owner (hex_user_id, package_id)
     VALUES ($1, $2)"
    |> pog.query
    |> pog.parameter(pog.int(u.id))
    |> pog.parameter(pog.int(package_id))
    |> pog.returning(dynamic.dynamic)
    |> pog.execute(db)
  })
  |> result.all()
  |> result.map_error(error.DatabaseError)
}

fn remove_old_package_owners(
  db: pog.Connection,
  owners: List(HexUser),
  current_owners: List(Int),
  package_id: Int,
) {
  let curr = list.map(owners, fn(o) { o.id })
  current_owners
  |> list.filter(fn(id) { list.contains(curr, id) })
  |> list.map(fn(u) {
    "DELETE FROM package_owner
     WHERE package_owner.hex_user_id = $1
       AND package_owner.package_id = $2"
    |> pog.query
    |> pog.parameter(pog.int(u))
    |> pog.parameter(pog.int(package_id))
    |> pog.returning(dynamic.dynamic)
    |> pog.execute(db)
  })
  |> result.all()
  |> result.map_error(error.DatabaseError)
}

pub fn sync_package_owners(
  db: pog.Connection,
  package_id: Int,
  owners: List(hexpm.PackageOwner),
) {
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
   RETURNING id"
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
  |> pog.returning(dynamic.field("id", dynamic.int))
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(response) {
    response.rows
    |> list.first()
    |> result.replace_error(error.UnknownError("Upsert package error"))
  })
}

pub fn upsert_release(
  db: pog.Connection,
  package_id: Int,
  release: hexpm.Release,
  package_interface: Option(String),
  gleam_toml: Option(String),
) {
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
   RETURNING id, package_interface, gleam_toml"
  |> pog.query
  |> pog.parameter(pog.int(package_id))
  |> pog.parameter(pog.text(release.version))
  |> pog.parameter(pog.text(release.url))
  |> pog.parameter(pog.nullable(pog.text, package_interface))
  |> pog.parameter(pog.nullable(pog.text, gleam_toml))
  |> pog.parameter({
    release.inserted_at
    |> birl.to_erlang_universal_datetime
    |> coerce
  })
  |> pog.returning(dynamic.decode3(
    fn(a, b, c) { #(a, b, c) },
    dynamic.field("id", dynamic.int),
    dynamic.field("package_interface", dynamic.optional(dynamic.string)),
    dynamic.field("gleam_toml", dynamic.optional(dynamic.string)),
  ))
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
}

pub fn lookup_release(
  db: pog.Connection,
  package_id: Int,
  release: hexpm.Release,
) {
  "SELECT id, package_interface, gleam_toml
  FROM package_release
  WHERE package_id = $1 AND version = $2"
  |> pog.query
  |> pog.parameter(pog.int(package_id))
  |> pog.parameter(pog.text(release.version))
  |> pog.returning(dynamic.decode3(
    fn(a, b, c) { #(a, b, c) },
    dynamic.field("id", dynamic.int),
    dynamic.field("package_interface", dynamic.optional(dynamic.string)),
    dynamic.field("gleam_toml", dynamic.optional(dynamic.string)),
  ))
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(res) {
    res.rows
    |> list.first()
    |> result.replace_error(error.UnknownError(""))
  })
}

pub fn add_package_gleam_constraint(
  db: pog.Connection,
  constraint: String,
  release_id: Int,
) {
  "UPDATE package_release SET gleam_constraint = $1 WHERE id = $2"
  |> pog.query
  |> pog.parameter(pog.text(constraint))
  |> pog.parameter(pog.int(release_id))
  |> pog.execute(db)
  |> result.replace(Nil)
  |> result.map_error(error.DatabaseError)
}

pub fn add_package_retirement(
  db: pog.Connection,
  retirement: hexpm.ReleaseRetirement,
  release_id: Int,
) {
  "UPDATE package_release SET retirement = $1 WHERE id = $2"
  |> pog.query
  |> pog.parameter(pog.text(encode_retirement(retirement)))
  |> pog.parameter(pog.int(release_id))
  |> pog.execute(db)
  |> result.replace(Nil)
  |> result.map_error(error.DatabaseError)
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
) {
  "SELECT
    package.id package_id,
    package_release.id package_release_id
  FROM package
  JOIN package_release
    ON package_release.package_id = package.id
  WHERE package.name = $1
    AND package_release.version = $2"
  |> pog.query
  |> pog.parameter(pog.text(package.name))
  |> pog.parameter(pog.text(package.version))
  |> pog.returning(dynamic.decode2(
    pair.new,
    dynamic.field("package_id", dynamic.int),
    dynamic.field("package_release_id", dynamic.int),
  ))
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(response) {
    list.first(response.rows)
    |> result.replace_error(error.UnknownError(
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
     RETURNING id"
    |> pog.query
    |> pog.parameter(pog.text(module.name))
    |> pog.parameter({
      module.module.documentation
      |> string.join("\n")
      |> pog.text()
    })
    |> pog.parameter(pog.int(module.release_id))
    |> pog.returning(dynamic.field("id", dynamic.int))
    |> pog.execute(db)
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
  db db: pog.Connection,
  kind kind: SignatureKind,
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
  "INSERT INTO package_type_fun_signature (
     name,
     documentation,
     signature_,
     json_signature,
     kind,
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
       kind = $5,
       parameters = $6,
       metadata = $7,
       deprecation = $9,
       implementations = $10
   RETURNING id"
  |> pog.query
  |> pog.parameter(pog.text(name))
  |> pog.parameter({
    documentation
    |> option.unwrap("")
    |> string.trim()
    |> pog.text()
  })
  |> pog.parameter(pog.text(signature))
  |> pog.parameter(json_signature |> json.to_string() |> pog.text())
  |> pog.parameter(pog.text(kind_to_string(kind)))
  |> pog.parameter(coerce(parameters))
  |> pog.parameter(metadata |> json.to_string() |> pog.text())
  |> pog.parameter(pog.int(module_id))
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
  |> pog.returning(dynamic.field("id", dynamic.int))
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
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
  |> pog.returning(dynamic.field("name", dynamic.string))
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn name_search(db: pog.Connection, query: String) {
  "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     string_to_array(regexp_replace(r.version, '([0-9]+).([0-9]+).([0-9]+).*', '\\1.\\2.\\3'), '.')::int[] AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   WHERE s.name = $1
   ORDER BY package_rank DESC, ordering DESC, type_name, signature_kind, module_name
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.text(query))
  |> pog.returning(type_search.decode)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn module_and_name_search(db: pog.Connection, query: String) {
  "WITH splitted_name AS (SELECT string_to_array($1, '.') AS full_name)
   SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     string_to_array(regexp_replace(r.version, '([0-9]+).([0-9]+).([0-9]+).*', '\\1.\\2.\\3'), '.')::int[] AS ordering
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
   ORDER BY package_rank DESC, ordering DESC, type_name, signature_kind, module_name
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.text(query))
  |> pog.returning(type_search.decode)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
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
  "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     string_to_array(regexp_replace(r.version, '([0-9]+).([0-9]+).([0-9]+).*', '\\1.\\2.\\3'), '.')::int[] AS ordering
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
      OR s.signature_ ILIKE '%' || $1 || '%'
      OR replace(s.signature_, ' ', '') ILIKE '%' || $1 || '%'
      OR replace(s.signature_, ' ', '') LIKE '%' || replace($2, ' ', '%') || '%'
    )
   ORDER BY package_rank DESC, ordering DESC, type_name, signature_kind, module_name
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.text(query))
  |> pog.parameter(pog.text(transform_query(query)))
  |> pog.returning(type_search.decode)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn signature_search(db: pog.Connection, q: String) {
  "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     string_to_array(regexp_replace(r.version, '([0-9]+).([0-9]+).([0-9]+).*', '\\1.\\2.\\3'), '.')::int[] AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   WHERE to_tsvector('english', s.signature_) @@ websearch_to_tsquery($1)
   ORDER BY package_rank DESC, ordering DESC, type_name, signature_kind, module_name
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.text(q))
  |> pog.returning(type_search.decode)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn documentation_search(db: pog.Connection, q: String) {
  "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     string_to_array(regexp_replace(r.version, '([0-9]+).([0-9]+).([0-9]+).*', '\\1.\\2.\\3'), '.')::int[] AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   WHERE to_tsvector('english', s.documentation) @@ websearch_to_tsquery($1)
   ORDER BY package_rank DESC, ordering DESC, type_name, signature_kind, module_name
   LIMIT 100"
  |> pog.query
  |> pog.parameter(pog.text(q))
  |> pog.returning(type_search.decode)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn module_search(db: pog.Connection, q: String) {
  "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     string_to_array(regexp_replace(r.version, '([0-9]+).([0-9]+).([0-9]+).*', '\\1.\\2.\\3'), '.')::int[] AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   WHERE m.name LIKE '%' || lower($1) || '%'
     AND r.version = (
       SELECT MAX(r.version)
       FROM package_release r
       JOIN package_module mod
         ON mod.package_release_id = r.id
       WHERE mod.name = m.name
         AND version SIMILAR TO '[0-9]*.[0-9]*.[0-9]*'
         AND r.package_id = p.id
         AND r.id = m.package_release_id
     )
   ORDER BY package_rank DESC, ordering DESC, type_name, signature_kind, module_name"
  |> pog.query
  |> pog.parameter(pog.text(q))
  |> pog.returning(type_search.decode)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn exact_type_search(db: pog.Connection, q: List(Int)) {
  use <- bool.guard(when: list.is_empty(q), return: Ok([]))
  let ids =
    list.index_map(q, fn(_, idx) { "$" <> int.to_string(idx + 1) })
    |> string.join(", ")
  {
    "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation documentation,
     s.kind signature_kind,
     s.metadata metadata,
     s.json_signature,
     m.name module_name,
     p.name package_name,
     r.version version,
     p.rank package_rank,
     string_to_array(regexp_replace(r.version, '([0-9]+).([0-9]+).([0-9]+).*', '\\1.\\2.\\3'), '.')::int[] AS ordering
   FROM package_type_fun_signature s
   JOIN package_module m
     ON m.id = s.package_module_id
   JOIN package_release r
     ON m.package_release_id = r.id
   JOIN package p
     ON p.id = r.package_id
   WHERE s.id IN ("
    <> ids
    <> ")
   ORDER BY package_rank DESC, ordering DESC, type_name, signature_kind, module_name"
  }
  |> pog.query
  |> list.fold(q, _, fn(query, q) { pog.parameter(query, pog.int(q)) })
  |> pog.returning(type_search.decode)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
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
  |> pog.returning(dynamic.field("gleam_toml", dynamic.string))
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn update_package_rank(db: pog.Connection, package: String, rank: Int) {
  "UPDATE package SET rank = $2 WHERE name = $1"
  |> pog.query
  |> pog.parameter(pog.text(package))
  |> pog.parameter(pog.int(rank))
  |> pog.returning(dynamic.dynamic)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
}

pub fn select_package_repository_address(db: pog.Connection, offset: Int) {
  "SELECT id, repository FROM package LIMIT 100 OFFSET $1"
  |> pog.query
  |> pog.parameter(pog.int(offset))
  |> pog.returning(fn(dyn) {
    dynamic.decode2(
      pair.new,
      dynamic.field("id", dynamic.int),
      dynamic.field("repository", dynamic.optional(dynamic.string)),
    )(dyn)
    |> result.map(fn(content) {
      case content {
        #(id, Some(repo)) -> Some(#(id, repo))
        #(_, None) -> None
      }
    })
  })
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn update_package_popularity(
  db: pog.Connection,
  url: String,
  popularity: Dict(String, Int),
) {
  "UPDATE package SET popularity = $2 WHERE repository = $1"
  |> pog.query
  |> pog.parameter(pog.text(url))
  |> pog.parameter({
    dict.to_list(popularity)
    |> list.map(pair.map_second(_, json.int))
    |> json.object()
    |> json.to_string()
    |> pog.text()
  })
  |> pog.returning(dynamic.dynamic)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
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
  |> pog.returning(package.decode)
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
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
  |> pog.returning(package.decode)
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

pub fn insert_analytics(
  db: pog.Connection,
  id: Int,
  table_name: String,
  content: Dict(String, Int),
) {
  "INSERT INTO analytics (foreign_id, table_name, content, day)
   VALUES ($1, $2, $3, $4)
   ON CONFLICT (foreign_id, table_name, day) DO UPDATE
     SET content = $3"
  |> pog.query
  |> pog.parameter(pog.int(id))
  |> pog.parameter(pog.text(table_name))
  |> pog.parameter({
    content
    |> dict.to_list()
    |> list.map(pair.map_second(_, json.int))
    |> json.object()
    |> json.to_string()
    |> pog.text()
  })
  |> pog.parameter({
    birl.now()
    |> birl.to_erlang_universal_datetime()
    |> pair.map_second(fn(_) { #(0, 0, 0) })
    |> coerce
  })
  |> pog.returning(dynamic.dynamic)
  |> pog.execute(db)
  |> result.map_error(error.DatabaseError)
}
