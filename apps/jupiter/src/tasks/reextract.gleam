//// One-shot repair: re-run signature extraction for the releases that still
//// hold recursive-type placeholders — rows whose second upsert never landed
//// (`signature_ = ''`, JSON `null` in `metadata`). Every
//// release keeps its raw `package_interface` and `gleam_toml`, so the whole
//// ingestion chain (`api/signatures.extract_signatures`) replays from the
//// database alone, with no call to Hex. Idempotent: the chain upserts.
////
//// Run with the real toolchain binaries and an explicit URL (mise shims
//// re-inject the developer's own `DATABASE_URL`):
////
////   DATABASE_URL=postgres://… gleam run -m tasks/reextract

import api/hex_repo
import api/signatures
import gleam/dynamic/decode
import gleam/int
import gleam/io
import gleam/list
import gleam/otp/static_supervisor as supervisor
import gleam/result
import gleam/string
import jupiter/context/postgres
import jupiter/gleam/context as gcontext
import pog

pub fn main() {
  let assert Ok(postgres) = postgres.supervised()
  let assert Ok(_) =
    supervisor.new(supervisor.OneForOne)
    |> supervisor.add(postgres)
    |> supervisor.start
  let db = postgres.connection()
  io.println("placeholders before: " <> int.to_string(count_placeholders(db)))
  let releases = releases_with_placeholders(db)
  io.println("releases to re-extract: " <> int.to_string(list.length(releases)))
  list.each(releases, fn(release) {
    let #(slug, interface, toml) = release
    case hex_repo.parse_files(interface, toml) {
      Error(error) ->
        io.println("  " <> slug <> ": cannot parse: " <> string.inspect(error))
      Ok(#(package, gleam_toml)) -> {
        let ctx = gcontext.Context(db:, package_interface: package, gleam_toml:)
        case signatures.extract_signatures(ctx) {
          Ok(_) -> io.println("  " <> slug <> ": ok")
          Error(error) ->
            io.println("  " <> slug <> ": failed: " <> string.inspect(error))
        }
      }
    }
  })
  io.println("placeholders after: " <> int.to_string(count_placeholders(db)))
}

fn count_placeholders(db: pog.Connection) -> Int {
  "SELECT count(*)::int AS n FROM package_type_fun_signature WHERE signature_ = ''"
  |> pog.query
  |> pog.returning(decode.at(["n"], decode.int))
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.unwrap([])
  |> list.first
  |> result.unwrap(-1)
}

/// `#(slug, package_interface, gleam_toml)` for every release with at least
/// one placeholder row and both files stored.
fn releases_with_placeholders(
  db: pog.Connection,
) -> List(#(String, String, String)) {
  "SELECT DISTINCT
     p.name || '@' || r.version AS slug,
     r.package_interface AS package_interface,
     r.gleam_toml AS gleam_toml
   FROM package_release r
   JOIN package p ON p.id = r.package_id
   JOIN package_module m ON m.package_release_id = r.id
   JOIN package_type_fun_signature s ON s.package_module_id = m.id
   WHERE s.signature_ = ''
     AND r.package_interface IS NOT NULL
     AND r.gleam_toml IS NOT NULL
   ORDER BY 1"
  |> pog.query
  |> pog.returning({
    use slug <- decode.field("slug", decode.string)
    use interface <- decode.field("package_interface", decode.string)
    use toml <- decode.field("gleam_toml", decode.string)
    decode.success(#(slug, interface, toml))
  })
  |> pog.execute(db)
  |> result.map(fn(r) { r.rows })
  |> result.unwrap([])
}
