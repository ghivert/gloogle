import backend/data/hex_read.{type HexRead, HexRead}
import backend/data/hex_user.{type HexUser}
import backend/error
import backend/gleam/context
import birl.{type Time}
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
import gleam/pgo
import gleam/result
import gleam/string
import helpers

pub type SignatureKind {
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
   OVERRIDING SYSTEM VALUE
   VALUES (1, $1)
   ON CONFLICT (id) DO UPDATE
     SET last_check = $1
   RETURNING *"
  |> pgo.execute(db, [timestamp], hex_read.decode)
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(response) {
    let err = "Upsert most recent hex timestamp failed"
    response.rows
    |> list.first()
    |> result.replace_error(error.UnknownError(err))
  })
}

pub fn upsert_search_analytics(db: pgo.Connection, query: String) {
  "INSERT INTO search_analytics (query)
   VALUES ($1)
   ON CONFLICT (query) DO UPDATE
     SET occurences = search_analytics.occurences + 1
   RETURNING *"
  |> pgo.execute(db, [pgo.text(query)], dynamic.dynamic)
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(response) {
    let err = "Upsert search analytics failed"
    response.rows
    |> list.first()
    |> result.replace_error(error.UnknownError(err))
  })
}

pub fn select_more_popular_packages(db: pgo.Connection) {
  let decoder =
    dynamic.tuple4(
      dynamic.string,
      dynamic.string,
      dynamic.int,
      dynamic.optional(dynamic.int),
    )
  use ranked <- result.try({
    "SELECT name, repository, rank, (popularity -> 'github')::int
    FROM package
    ORDER BY rank DESC
    LIMIT 22"
    |> pgo.execute(db, [], decoder)
    |> result.map(fn(r) { r.rows })
    |> result.map_error(error.DatabaseError)
  })
  use popular <- result.try({
    "SELECT name, repository, rank, (popularity -> 'github')::int
    FROM package
    WHERE popularity -> 'github' IS NOT NULL
      AND name != 'funtil'
      AND name != 'dew'
    ORDER BY popularity -> 'github' DESC
    LIMIT 23"
    |> pgo.execute(db, [], decoder)
    |> result.map(fn(r) { r.rows })
    |> result.map_error(error.DatabaseError)
  })
  Ok(#(ranked, popular))
}

pub fn select_last_day_search_analytics(db: pgo.Connection) {
  let #(date, _) = birl.to_erlang_universal_datetime(birl.now())
  let now = birl.from_erlang_universal_datetime(#(date, #(0, 0, 0)))
  "SELECT query, occurences
   FROM search_analytics
   WHERE updated_at >= $1"
  |> pgo.execute(
    db,
    [helpers.convert_time(now)],
    dynamic.tuple2(dynamic.string, dynamic.int),
  )
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

pub fn upsert_search_analytics_timeseries(
  db: pgo.Connection,
  analytic: #(String, Int),
) {
  let #(date, _) = birl.to_erlang_universal_datetime(birl.now())
  let now = birl.from_erlang_universal_datetime(#(date, #(0, 0, 0)))
  let timestamp = helpers.convert_time(now)
  let #(query, occurences) = analytic
  "INSERT INTO analytics_timeseries (query, occurences, date)
   VALUES ($1, $2, $3)
   ON CONFLICT (query, date) DO UPDATE
     SET occurences = $2"
  |> pgo.execute(
    db,
    [pgo.text(query), pgo.int(occurences), timestamp],
    dynamic.dynamic,
  )
  |> result.map_error(error.DatabaseError)
}

pub fn get_timeseries_count(db: pgo.Connection) {
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
  |> pgo.execute(db, [], dynamic.tuple2(dynamic.int, helpers.decode_time))
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
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

pub fn get_total_searches(db: pgo.Connection) {
  "SELECT SUM(occurences) FROM search_analytics"
  |> pgo.execute(db, [], dynamic.element(0, dynamic.int))
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

pub fn get_total_signatures(db: pgo.Connection) {
  "SELECT COUNT(*) FROM package_type_fun_signature"
  |> pgo.execute(db, [], dynamic.element(0, dynamic.int))
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

pub fn get_total_packages(db: pgo.Connection) {
  "SELECT COUNT(*) FROM package"
  |> pgo.execute(db, [], dynamic.element(0, dynamic.int))
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
  package_interface: Option(String),
  gleam_toml: Option(String),
) {
  let package_id = pgo.int(package_id)
  let version = pgo.text(release.version)
  let url = pgo.text(release.url)
  let package_interface = pgo.nullable(pgo.text, package_interface)
  let gleam_toml = pgo.nullable(pgo.text, gleam_toml)
  let inserted_at =
    release.inserted_at
    |> birl.to_erlang_universal_datetime
    |> dynamic.from
    |> dynamic.unsafe_coerce
  let args = [
    package_id,
    version,
    url,
    package_interface,
    gleam_toml,
    inserted_at,
  ]
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
  |> pgo.execute(
    db,
    args,
    dynamic.tuple3(
      dynamic.int,
      dynamic.optional(dynamic.string),
      dynamic.optional(dynamic.string),
    ),
  )
  |> result.map_error(error.DatabaseError)
}

pub fn lookup_release(
  db: pgo.Connection,
  package_id: Int,
  release: hexpm.Release,
) {
  let package_id = pgo.int(package_id)
  let version = pgo.text(release.version)
  let args = [package_id, version]
  "SELECT id, package_interface, gleam_toml
   FROM package_release
   WHERE package_id = $1 AND version = $2"
  |> pgo.execute(
    db,
    args,
    dynamic.tuple3(
      dynamic.int,
      dynamic.optional(dynamic.string),
      dynamic.optional(dynamic.string),
    ),
  )
  |> result.map_error(error.DatabaseError)
  |> result.try(fn(res) {
    res.rows
    |> list.first()
    |> result.replace_error(error.UnknownError(""))
  })
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

pub fn add_package_gleam_retirement(
  db: pgo.Connection,
  retirement: hexpm.ReleaseRetirement,
  release_id: Int,
) {
  let retirement =
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
    |> pgo.text()
  let release_id = pgo.int(release_id)
  "UPDATE package_release SET retirement = $1 WHERE id = $2"
  |> pgo.execute(db, [retirement, release_id], dynamic.dynamic)
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
  let kind = case kind {
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
      pgo.text(kind),
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
    dynamic.element(0, dynamic.int),
  )
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn find_similar_type_names(db: pgo.Connection, name: String) {
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
  |> pgo.execute(db, [pgo.text(name)], dynamic.element(0, dynamic.string))
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn name_search(db: pgo.Connection, query: String) {
  let query = pgo.text(query)
  "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation,
     s.kind signature_kind,
     s.metadata,
     s.json_signature,
     m.name module_name,
     p.name,
     r.version,
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
  |> pgo.execute(db, [query], decode_type_search)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn module_and_name_search(db: pgo.Connection, query: String) {
  let query = pgo.text(query)
  "WITH splitted_name AS (SELECT string_to_array($1, '.') AS full_name)
   SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation,
     s.kind signature_kind,
     s.metadata,
     s.json_signature,
     m.name module_name,
     p.name,
     r.version,
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
  |> pgo.execute(db, [query], decode_type_search)
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

pub fn content_search(db: pgo.Connection, query: String) {
  let pattern = pgo.text(transform_query(query))
  let query = pgo.text(query)
  "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation,
     s.kind signature_kind,
     s.metadata,
     s.json_signature,
     m.name module_name,
     p.name,
     r.version,
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
  |> pgo.execute(db, [query, pattern], decode_type_search)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

fn decode_type_search(dyn) {
  dynamic.decode8(
    fn(a, b, c, d, e, f, g, h) { #(a, b, c, d, e, f, g, h) },
    dynamic.element(0, dynamic.string),
    dynamic.element(1, dynamic.string),
    dynamic.element(2, dynamic.string),
    dynamic.element(3, dynamic.dynamic),
    dynamic.element(4, dynamic.dynamic),
    dynamic.element(5, dynamic.string),
    dynamic.element(6, dynamic.string),
    dynamic.element(7, dynamic.string),
  )(dyn)
}

pub fn type_search_to_json(item) {
  let #(a, b, c, d, e, f, g, h) = item
  json.object([
    #("name", json.string(a)),
    #("documentation", json.string(b)),
    #("kind", json.string(c)),
    #("metadata", dynamic.unsafe_coerce(d)),
    #("json_signature", dynamic.unsafe_coerce(e)),
    #("module_name", json.string(f)),
    #("package_name", json.string(g)),
    #("version", json.string(h)),
  ])
}

pub fn signature_search(db: pgo.Connection, q: String) {
  let query = pgo.text(q)
  "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation,
     s.kind signature_kind,
     s.metadata,
     s.json_signature,
     m.name module_name,
     p.name,
     r.version,
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
  |> pgo.execute(db, [query], decode_type_search)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn documentation_search(db: pgo.Connection, q: String) {
  let query = pgo.text(q)
  "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation,
     s.kind signature_kind,
     s.metadata,
     s.json_signature,
     m.name module_name,
     p.name,
     r.version,
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
  |> pgo.execute(db, [query], decode_type_search)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn module_search(db: pgo.Connection, q: String) {
  let query = pgo.text(q)
  "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation,
     s.kind signature_kind,
     s.metadata,
     s.json_signature,
     m.name module_name,
     p.name,
     r.version,
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
  |> pgo.execute(db, [query], decode_type_search)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn exact_type_search(db: pgo.Connection, q: List(Int)) {
  use <- bool.guard(when: list.is_empty(q), return: Ok([]))
  let ids =
    list.index_map(q, fn(_, idx) { "$" <> int.to_string(idx + 1) })
    |> string.join(", ")
  {
    "SELECT DISTINCT ON (package_rank, ordering, type_name, signature_kind, module_name)
     s.name type_name,
     s.documentation,
     s.kind signature_kind,
     s.metadata,
     s.json_signature,
     m.name module_name,
     p.name,
     r.version,
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
  |> pgo.execute(db, list.map(q, pgo.int), decode_type_search)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn select_gleam_toml(db: pgo.Connection, offset: Int) {
  "SELECT gleam_toml
   FROM package_release
   WHERE gleam_toml IS NOT NULL
   ORDER BY id
   LIMIT 100
   OFFSET $1"
  |> pgo.execute(db, [pgo.int(offset)], dynamic.element(0, dynamic.string))
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn update_package_rank(db: pgo.Connection, package: String, rank: Int) {
  "UPDATE package SET rank = $2 WHERE name = $1"
  |> pgo.execute(db, [pgo.text(package), pgo.int(rank)], dynamic.dynamic)
  |> result.map_error(error.DatabaseError)
}

pub fn select_package_repository_address(db: pgo.Connection, offset: Int) {
  let decoder = fn(dyn) {
    dynamic.tuple2(dynamic.int, dynamic.optional(dynamic.string))(dyn)
    |> result.map(fn(content) {
      case content {
        #(id, Some(repo)) -> Some(#(id, repo))
        #(_, None) -> None
      }
    })
  }
  "SELECT id, repository FROM package LIMIT 100 OFFSET $1"
  |> pgo.execute(db, [pgo.int(offset)], decoder)
  |> result.map_error(error.DatabaseError)
  |> result.map(fn(r) { r.rows })
}

pub fn update_package_popularity(
  db: pgo.Connection,
  url: String,
  popularity: Dict(String, Int),
) {
  let popularity =
    dict.to_list(popularity)
    |> list.map(pair.map_second(_, json.int))
    |> json.object()
    |> json.to_string()
    |> pgo.text()
  "UPDATE package SET popularity = $2 WHERE repository = $1"
  |> pgo.execute(db, [pgo.text(url), popularity], dynamic.dynamic)
  |> result.map_error(error.DatabaseError)
}

pub fn select_package_by_popularity(db: pgo.Connection, page: Int) {
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
  |> pgo.execute(
    db,
    [pgo.int(offset)],
    dynamic.decode8(
      fn(a, b, c, d, e, f, g, h) {
        json.object([
          #("name", json.string(a)),
          #("repository", json.nullable(b, json.string)),
          #("documentation", json.nullable(c, json.string)),
          #("hex-url", json.nullable(d, json.string)),
          #("licenses", json.string(e)),
          #("description", json.nullable(f, json.string)),
          #("rank", json.int(g)),
          #("popularity", json.nullable(h, json.string)),
        ])
      },
      dynamic.element(0, dynamic.string),
      dynamic.element(1, dynamic.optional(dynamic.string)),
      dynamic.element(2, dynamic.optional(dynamic.string)),
      dynamic.element(3, dynamic.optional(dynamic.string)),
      dynamic.element(4, dynamic.string),
      dynamic.element(5, dynamic.optional(dynamic.string)),
      dynamic.element(6, dynamic.int),
      dynamic.element(7, dynamic.optional(dynamic.string)),
    ),
  )
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

pub fn select_package_by_updated_at(db: pgo.Connection) {
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
  |> pgo.execute(
    db,
    [],
    dynamic.decode8(
      fn(a, b, c, d, e, f, g, h) {
        json.object([
          #("name", json.string(a)),
          #("repository", json.nullable(b, json.string)),
          #("documentation", json.nullable(c, json.string)),
          #("hex-url", json.nullable(d, json.string)),
          #("licenses", json.string(e)),
          #("description", json.nullable(f, json.string)),
          #("rank", json.int(g)),
          #("popularity", json.nullable(h, json.string)),
        ])
      },
      dynamic.element(0, dynamic.string),
      dynamic.element(1, dynamic.optional(dynamic.string)),
      dynamic.element(2, dynamic.optional(dynamic.string)),
      dynamic.element(3, dynamic.optional(dynamic.string)),
      dynamic.element(4, dynamic.string),
      dynamic.element(5, dynamic.optional(dynamic.string)),
      dynamic.element(6, dynamic.int),
      dynamic.element(7, dynamic.optional(dynamic.string)),
    ),
  )
  |> result.map(fn(r) { r.rows })
  |> result.map_error(error.DatabaseError)
}

pub fn insert_analytics(
  db: pgo.Connection,
  id: Int,
  table_name: String,
  content: Dict(String, Int),
) {
  let day =
    birl.now()
    |> birl.to_erlang_universal_datetime()
    |> pair.map_second(fn(_) { #(0, 0, 0) })
    |> dynamic.from()
    |> dynamic.unsafe_coerce()
  let content =
    content
    |> dict.to_list()
    |> list.map(pair.map_second(_, json.int))
    |> json.object()
    |> json.to_string()
    |> pgo.text()
  let parameters = [pgo.int(id), pgo.text(table_name), content, day]
  "INSERT INTO analytics (foreign_id, table_name, content, day)
   VALUES ($1, $2, $3, $4)
   ON CONFLICT (foreign_id, table_name, day) DO UPDATE
     SET content = $3"
  |> pgo.execute(db, parameters, dynamic.dynamic)
  |> result.map_error(error.DatabaseError)
}
