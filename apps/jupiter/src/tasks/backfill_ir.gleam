//// One-shot backfill of the IR-derived columns — `signature_ir`, `arity`,
//// `type_names`, `type_names_key`, `signature_canonical` — for rows ingested
//// before those columns existed.
////
//// Every release keeps its raw `package_interface` JSON, so the backfill is
//// pure re-derivation: decode the interface, lower each declaration with
//// `data/ir/from_gleam`, run `api/signatures.structured` (the one place the
//// three columns are derived from the canonical declaration), and UPDATE the
//// row matched by `(package_module_id, name, kind)`. Nothing is re-fetched from
//// Hex and no other column is touched, so re-running it is harmless.
////
//// Releases are walked by keyset on `package_release.id`, one transaction per
//// release. Run it with the real toolchain binaries and an explicit URL —
//// mise shims re-inject the developer's own `DATABASE_URL`:
////
////   DATABASE_URL=postgres://… gleam run -m tasks/backfill_ir

import api/signatures
import data/ir/from_gleam
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/otp/static_supervisor as supervisor
import gleam/package_interface as pi
import gleam/result
import gleam/string
import jupiter/context/postgres
import jupiter/postgres/queries
import pog

const page_size = 100

pub type Stats {
  Stats(
    releases: Int,
    updated: Int,
    unmatched: Int,
    undecodable: Int,
    failed: Int,
  )
}

pub fn main() {
  let assert Ok(postgres) = postgres.supervised()
  let assert Ok(_) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(postgres)
    |> supervisor.start
  let db = postgres.connection()
  let stats = backfill(db, "", Stats(0, 0, 0, 0, 0))
  io.println("backfill done")
  report(stats)
}

fn report(stats: Stats) {
  io.println(
    "  releases="
    <> int.to_string(stats.releases)
    <> " updated="
    <> int.to_string(stats.updated)
    <> " unmatched="
    <> int.to_string(stats.unmatched)
    <> " undecodable="
    <> int.to_string(stats.undecodable)
    <> " failed="
    <> int.to_string(stats.failed),
  )
}

/// Walk releases from `after` (exclusive, `""` to start), page by page.
fn backfill(db: pog.Connection, after: String, stats: Stats) -> Stats {
  case releases_after(db, after) {
    Error(error) -> {
      io.println("page query failed after " <> after <> ": " <> error)
      stats
    }
    Ok([]) -> stats
    Ok(releases) -> {
      let stats =
        list.fold(releases, stats, fn(stats, release) {
          let #(id, interface) = release
          let stats = backfill_release(db, id, interface, stats)
          case stats.releases % 500 {
            0 -> report(stats)
            _ -> Nil
          }
          stats
        })
      let assert Ok(#(last, _)) = list.last(releases)
      backfill(db, last, stats)
    }
  }
}

fn backfill_release(
  db: pog.Connection,
  release_id: String,
  interface: String,
  stats: Stats,
) -> Stats {
  let stats = Stats(..stats, releases: stats.releases + 1)
  case json.parse(interface, pi.decoder()) {
    Error(_) -> Stats(..stats, undecodable: stats.undecodable + 1)
    Ok(package) -> {
      // Read the module map on the pool, not inside the transaction: pog's
      // checked-out transaction connection returns rows as tuples, not maps,
      // so a by-name decoder fails there. The UPDATEs decode nothing.
      let outcome = {
        use modules <- result.try(
          modules_of(db, release_id)
          |> result.map_error(pog.TransactionQueryError),
        )
        use db <- pog.transaction(db)
        package.modules
        |> dict.to_list
        |> list.try_fold(#(0, 0), fn(acc, entry) {
          let #(name, module) = entry
          case dict.get(modules, name) {
            // A module in the interface with no row: never ingested, skip.
            Error(_) -> Ok(acc)
            Ok(module_id) -> backfill_module(db, module_id, module, acc)
          }
        })
      }
      case outcome {
        Ok(#(updated, unmatched)) ->
          Stats(
            ..stats,
            updated: stats.updated + updated,
            unmatched: stats.unmatched + unmatched,
          )
        Error(error) -> {
          io.println("release " <> release_id <> ": " <> string.inspect(error))
          Stats(..stats, failed: stats.failed + 1)
        }
      }
    }
  }
}

/// Every declaration of a module, lowered and written. Returns the running
/// `#(updated, unmatched)` counters.
fn backfill_module(
  db: pog.Connection,
  module_id: String,
  module: pi.Module,
  acc: #(Int, Int),
) -> Result(#(Int, Int), pog.QueryError) {
  let declarations =
    list.flatten([
      dict.to_list(module.types)
        |> list.map(fn(e) {
          #(e.0, "type_definition", from_gleam.type_definition(e.1))
        }),
      dict.to_list(module.type_aliases)
        |> list.map(fn(e) { #(e.0, "type_alias", from_gleam.type_alias(e.1)) }),
      dict.to_list(module.constants)
        |> list.map(fn(e) { #(e.0, "constant", from_gleam.constant(e.1)) }),
      dict.to_list(module.functions)
        |> list.map(fn(e) { #(e.0, "function", from_gleam.function(e.1)) }),
    ])
  use acc, #(name, kind, declaration) <- list.try_fold(declarations, acc)
  let derived = signatures.structured(declaration)
  use count <- result.map(update_row(db, module_id, name, kind, derived))
  case count {
    0 -> #(acc.0, acc.1 + 1)
    _ -> #(acc.0 + count, acc.1)
  }
}

fn update_row(
  db: pog.Connection,
  module_id: String,
  name: String,
  kind: String,
  derived: signatures.Derived,
) -> Result(Int, pog.QueryError) {
  let signatures.Derived(signature_ir:, arity:, type_names:, canonical:) =
    derived
  "UPDATE package_type_fun_signature
   SET signature_ir = $1, arity = $2, type_names = $3, type_names_key = $7,
       signature_canonical = $8
   WHERE package_module_id = $4 AND name = $5 AND kind = $6"
  |> pog.query
  |> pog.parameter(signature_ir |> json.to_string |> pog.text)
  |> pog.parameter(pog.int(arity))
  |> pog.parameter(pog.array(pog.text, type_names))
  |> pog.parameter(pog.text(module_id))
  |> pog.parameter(pog.text(name))
  |> pog.parameter(pog.text(kind))
  |> pog.parameter(pog.text(queries.type_names_key(type_names)))
  |> pog.parameter(pog.nullable(pog.text, canonical))
  |> pog.returning(decode.success(Nil))
  |> pog.execute(db)
  |> result.map(fn(r) { r.count })
}

/// `module name → module id` for one release.
fn modules_of(
  db: pog.Connection,
  release_id: String,
) -> Result(Dict(String, String), pog.QueryError) {
  "SELECT id::text AS id, name
   FROM package_module
   WHERE package_release_id = $1"
  |> pog.query
  |> pog.parameter(pog.text(release_id))
  |> pog.returning({
    use id <- decode.field("id", decode.string)
    use name <- decode.field("name", decode.string)
    decode.success(#(name, id))
  })
  |> pog.execute(db)
  |> result.map(fn(r) { dict.from_list(r.rows) })
}

/// One keyset page of releases that stored their interface. uuid order and
/// the canonical text order agree, so the cursor is the id as text.
fn releases_after(
  db: pog.Connection,
  after: String,
) -> Result(List(#(String, String)), String) {
  "SELECT id::text AS id, package_interface
   FROM package_release
   WHERE package_interface IS NOT NULL
     AND ($1 = '' OR id::text > $1)
   ORDER BY id
   LIMIT $2"
  |> pog.query
  |> pog.parameter(pog.text(after))
  |> pog.parameter(pog.int(page_size))
  |> pog.returning({
    use id <- decode.field("id", decode.string)
    use interface <- decode.field("package_interface", decode.string)
    decode.success(#(id, interface))
  })
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(fn(e) { pog_error(e) })
}

fn pog_error(error: pog.QueryError) -> String {
  case error {
    pog.PostgresqlError(code, name, message) ->
      code <> " " <> name <> ": " <> message
    other -> string.inspect(other)
  }
}
