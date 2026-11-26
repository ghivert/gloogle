//// Type links, resolved at query time.
////
//// A `Named` in a stored signature carries the package and module that define
//// it (`gleam_stdlib`, `gleam/option`), but not a version: the version a
//// release refers to is whichever release of that package satisfies the
//// requirement *this* release declares in its `gleam.toml` — a semver range
//// against an evolving set of published versions, so it is resolved here,
//// against the releases known today, and never stored (`signature-ir.md`,
//// cross-reference resolution).
////
//// For a batch of results: collect the packages their signatures mention,
//// load the declaring releases' dependencies (`package_dependency`) and the
//// candidate versions of those packages, then pick, per result and per
//// package, the newest version matching the requirement. The result goes in
//// `TypeSearch.links` (`package → version`); venus turns it into hexdocs
//// links. A package the release does not declare (a transitive dependency
//// whose type leaks into a signature) gets no link, by design: a guessed
//// version is worse than none. A result's own package needs no entry — its
//// version is the result's.

import data/ir
import data/type_search.{type TypeSearch}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option
import gleam/result
import gleam/semver
import jupiter/loss
import jupiter/postgres/queries
import pog

/// Fill `links` on every result. On a database error the results come back
/// unlinked, and the error is logged.
pub fn attach(db: pog.Connection, items: List(TypeSearch)) -> List(TypeSearch) {
  let wanted =
    list.filter_map(items, fn(item) {
      case foreign_packages(item) {
        [] -> Error(Nil)
        packages -> Ok(#(#(item.package_name, item.version), packages))
      }
    })
  case wanted {
    [] -> items
    _ -> {
      let releases = list.map(wanted, fn(w) { w.0 }) |> list.unique
      let resolved = {
        use dependencies <- result.try(queries.dependencies_of(db, releases))
        let requirements = requirements_by_release(dependencies)
        let packages =
          wanted
          |> list.flat_map(fn(w) {
            let needed =
              dict.get(requirements, w.0) |> result.unwrap(dict.new())
            list.filter(w.1, dict.has_key(needed, _))
          })
          |> list.unique
        use versions <- result.map(queries.releases_of(db, packages))
        #(requirements, versions_by_package(versions))
      }
      case resolved {
        Error(error) -> {
          loss.log(error)
          items
        }
        Ok(#(requirements, versions)) ->
          list.map(items, fn(item) {
            let needed =
              dict.get(requirements, #(item.package_name, item.version))
              |> result.unwrap(dict.new())
            let links =
              foreign_packages(item)
              |> list.filter_map(fn(package) {
                use requirement <- result.try(dict.get(needed, package))
                use candidates <- result.try(dict.get(versions, package))
                use version <- result.map(
                  list.find(candidates, matches(_, requirement)),
                )
                #(package, version)
              })
              |> dict.from_list
            type_search.TypeSearch(..item, links:)
          })
      }
    }
  }
}

/// The packages a result's signature mentions, other than its own and the
/// prelude (whose types carry no package).
fn foreign_packages(item: TypeSearch) -> List(String) {
  item.signature_ir
  |> packages_of
  |> list.filter(fn(package) { package != "" && package != item.package_name })
  |> list.unique
}

fn packages_of(declaration: ir.Declaration) -> List(String) {
  case declaration {
    ir.Function(signature:) -> collect(signature, [])
    ir.Constant(type_:) -> collect(type_, [])
    ir.TypeAlias(aliased:, ..) -> collect(aliased, [])
    ir.TypeDefinition(constructors:, ..) ->
      list.fold(constructors, [], fn(acc, constructor) {
        list.fold(constructor.parameters, acc, fn(acc, param) {
          collect(param.type_, acc)
        })
      })
  }
}

fn collect(type_: ir.Type, acc: List(String)) -> List(String) {
  case type_ {
    ir.Named(name:, args:) ->
      list.fold(args, [name.package, ..acc], fn(acc, arg) { collect(arg, acc) })
    ir.Fn(params:, return:, ..) ->
      params
      |> list.fold(acc, fn(acc, param) { collect(param.type_, acc) })
      |> collect(return, _)
    ir.Tuple(elements:) ->
      list.fold(elements, acc, fn(acc, element) { collect(element, acc) })
    ir.Union(members:) ->
      list.fold(members, acc, fn(acc, member) { collect(member, acc) })
    ir.Record(fields:) ->
      list.fold(fields, acc, fn(acc, field) { collect(field.type_, acc) })
    ir.Var(..) | ir.Hole -> acc
  }
}

/// `(package, version) → (dependency → requirement)`; the rows come runtime
/// dependencies first, and the first requirement seen for a dependency wins.
fn requirements_by_release(
  rows: List(#(String, String, String, String)),
) -> Dict(#(String, String), Dict(String, String)) {
  list.fold(rows, dict.new(), fn(acc, row) {
    let #(package, version, dependency, requirement) = row
    dict.upsert(acc, #(package, version), fn(existing) {
      let existing = option.unwrap(existing, dict.new())
      case dict.has_key(existing, dependency) {
        True -> existing
        False -> dict.insert(existing, dependency, requirement)
      }
    })
  })
}

/// `package → versions`, newest first (the query's order).
fn versions_by_package(
  rows: List(#(String, String)),
) -> Dict(String, List(String)) {
  rows
  |> list.group(fn(row) { row.0 })
  |> dict.map_values(fn(_, rows) {
    rows |> list.map(fn(row) { row.1 }) |> list.reverse
  })
}

fn matches(version: String, requirement: String) -> Bool {
  semver.is_match(<<version:utf8>>, <<requirement:utf8>>) == Ok(True)
}
