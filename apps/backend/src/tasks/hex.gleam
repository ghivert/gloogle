import backend/config.{type Config}
import backend/data/hex_read.{type HexRead}
import backend/index/connect as postgres
import backend/index/error.{type Error}
import backend/index/queries as index
import birl.{type Time}
import birl/duration
import gleam/dynamic
import gleam/hackney
import gleam/hexpm.{type Package}
import gleam/http/request

import gleam/int
import gleam/io
import gleam/json
import gleam/list
import gleam/order
import gleam/pgo
import gleam/result
import gleam/string
import gleam/uri
import wisp

type State {
  State(
    page: Int,
    limit: Time,
    newest: Time,
    hex_api_key: String,
    last_logged: Time,
    db: pgo.Connection,
  )
}

pub fn sync_new_gleam_releases(cnf: Config) -> Result(HexRead, Error) {
  let ctx = postgres.connect(cnf)
  wisp.log_info("Syncing new releases from Hex")
  use limit <- result.try(index.get_last_hex_date(ctx.connection))
  use latest <- result.try(
    sync_packages(State(
      page: 1,
      limit: limit,
      newest: limit,
      hex_api_key: cnf.hex_api_key,
      last_logged: birl.now(),
      db: ctx.connection,
    )),
  )
  let latest = index.upsert_most_recent_hex_timestamp(ctx.connection, latest)
  wisp.log_info("\nUp to date!")
  latest
}

fn keep_newest_date(package: hexpm.Package, state: State) {
  case birl.compare(package.updated_at, state.newest) {
    order.Gt -> package.updated_at
    _ -> state.newest
  }
}

fn first_timestamp(packages: List(hexpm.Package), state: State) -> Time {
  packages
  |> list.first()
  |> result.map(keep_newest_date(_, state))
  |> result.unwrap(state.newest)
}

fn sync_packages(state: State) -> Result(Time, Error) {
  use all_packages <- result.try(get_api_packages_page(state))
  let state = State(..state, newest: first_timestamp(all_packages, state))
  let new_packages = take_fresh_packages(all_packages, state.limit)
  use state <- result.try(list.try_fold(new_packages, state, sync_package))
  case list.length(all_packages) == list.length(new_packages) {
    _ if all_packages == [] -> Ok(state.newest)
    False -> Ok(state.newest)
    True -> sync_packages(State(..state, page: state.page + 1))
  }
}

fn sync_package(state: State, package: hexpm.Package) -> Result(State, Error) {
  let secret = state.hex_api_key
  let db = state.db
  use releases <- result.try(lookup_gleam_releases(package, secret: secret))
  case releases {
    [] -> Ok(log_if_needed(state, package.updated_at))
    _ -> {
      use _ <- result.map(insert_package_and_releases(package, releases, db))
      State(..state, last_logged: birl.now())
    }
  }
}

fn insert_package_and_releases(
  package: hexpm.Package,
  releases: List(hexpm.Release),
  db: pgo.Connection,
) {
  let versions =
    releases
    |> list.map(fn(release) { release.version })
    |> string.join(", v")
  wisp.log_info("Saving " <> package.name <> " v" <> versions)
  use id <- result.try(index.upsert_package(db, package))
  use _ <- result.try(index.sync_package_owners(db, package))
  list.try_each(releases, fn(r) { index.upsert_release(db, id, r) })
}

fn lookup_gleam_releases(
  package: hexpm.Package,
  secret hex_api_key: String,
) -> Result(List(hexpm.Release), Error) {
  let lookup = list.try_map(package.releases, lookup_release(_, hex_api_key))
  use releases <- result.map(lookup)
  list.filter(releases, fn(r) { list.contains(r.meta.build_tools, "gleam") })
}

fn lookup_release(
  release: hexpm.PackageRelease,
  secret hex_api_key: String,
) -> Result(hexpm.Release, Error) {
  let assert Ok(url) = uri.parse(release.url)

  use response <- result.try(
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path(url.path)
    |> request.prepend_header("authorization", hex_api_key)
    |> hackney.send
    |> result.map_error(error.FetchError),
  )

  response.body
  |> json.decode(using: hexpm.decode_release)
  |> result.map_error(error.JsonError)
}

fn log_if_needed(state: State, time: Time) -> State {
  let interval = duration.new([#(5, duration.Second)])
  let print_deadline = birl.add(state.last_logged, interval)
  case birl.compare(print_deadline, birl.now()) == order.Lt {
    False -> state
    True -> {
      wisp.log_info("Still syncing, up to " <> birl.to_iso8601(time))
      State(..state, last_logged: birl.now())
    }
  }
}

pub fn take_fresh_packages(packages: List(Package), limit: Time) {
  use package <- list.take_while(packages)
  birl.compare(limit, package.updated_at) == order.Lt
}

fn get_api_packages_page(state: State) -> Result(List(hexpm.Package), Error) {
  use response <- result.try(
    request.new()
    |> request.set_host("hex.pm")
    |> request.set_path("/api/packages")
    |> request.set_query([
      #("sort", "updated_at"),
      #("page", int.to_string(state.page)),
    ])
    |> request.prepend_header("authorization", state.hex_api_key)
    |> hackney.send
    |> result.map_error(error.FetchError),
  )

  response.body
  |> json.decode(using: dynamic.list(of: hexpm.decode_package))
  |> result.map_error(error.JsonError)
}
