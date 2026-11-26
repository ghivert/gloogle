//// One-shot backfill of `package_dependency` for the releases ingested before
//// the table existed. Every release keeps its raw `gleam.toml`, so this is
//// pure re-derivation: parse it, take `toml.dependencies`, upsert the rows.
//// Idempotent (the upsert replaces the requirement). Run with the real
//// toolchain binaries, an explicit URL and `GLEAM_ENV=development` on the
//// local stack:
////
////   GLEAM_ENV=development DATABASE_URL=postgres://… gleam run -m tasks/backfill_dependencies

import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/list
import gleam/otp/static_supervisor as supervisor
import gleam/result
import jupiter/context/postgres
import jupiter/gleam/toml
import jupiter/postgres/queries
import pog
import tom

const page_size = 200

pub type Stats {
  Stats(releases: Int, dependencies: Int, unparsable: Int, failed: Int)
}

pub fn main() {
  let assert Ok(postgres) = postgres.supervised()
  let assert Ok(_) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(postgres)
    |> supervisor.start
  let db = postgres.connection()
  let stats = backfill(db, "", Stats(0, 0, 0, 0))
  io.println(
    "backfill done: releases="
    <> int.to_string(stats.releases)
    <> " dependencies="
    <> int.to_string(stats.dependencies)
    <> " unparsable="
    <> int.to_string(stats.unparsable)
    <> " failed="
    <> int.to_string(stats.failed),
  )
}

fn backfill(db: pog.Connection, after: String, stats: Stats) -> Stats {
  case releases_after(db, after) {
    Error(error) -> {
      io.println("page query failed after " <> after <> ": " <> error)
      stats
    }
    Ok([]) -> stats
    Ok(page) -> {
      let stats =
        list.fold(page, stats, fn(stats, release) {
          backfill_release(db, release.0, release.1, stats)
        })
      let assert Ok(last) = list.last(page)
      backfill(db, last.0, stats)
    }
  }
}

fn backfill_release(
  db: pog.Connection,
  release_id: String,
  gleam_toml: String,
  stats: Stats,
) -> Stats {
  let stats = Stats(..stats, releases: stats.releases + 1)
  case tom.parse(gleam_toml) {
    Error(_) -> Stats(..stats, unparsable: stats.unparsable + 1)
    Ok(parsed) -> {
      let dependencies = toml.dependencies(parsed)
      let outcome =
        list.try_each(dependencies, queries.upsert_package_dependency(
          db,
          release_id,
          _,
        ))
      case outcome {
        Ok(_) ->
          Stats(
            ..stats,
            dependencies: stats.dependencies + list.length(dependencies),
          )
        Error(_) -> Stats(..stats, failed: stats.failed + 1)
      }
    }
  }
}

/// One keyset page of releases that stored their `gleam.toml`.
fn releases_after(
  db: pog.Connection,
  after: String,
) -> Result(List(#(String, String)), String) {
  "SELECT id::text AS id, gleam_toml
   FROM package_release
   WHERE gleam_toml IS NOT NULL
     AND ($1 = '' OR id::text > $1)
   ORDER BY id
   LIMIT $2"
  |> pog.query
  |> pog.parameter(pog.text(after))
  |> pog.parameter(pog.int(page_size))
  |> pog.returning({
    use id <- decode.field("id", decode.string)
    use gleam_toml <- decode.field("gleam_toml", decode.string)
    decode.success(#(id, gleam_toml))
  })
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.map_error(fn(e) {
    case e {
      pog.PostgresqlError(_, _, message) -> message
      _ -> "query error"
    }
  })
}
