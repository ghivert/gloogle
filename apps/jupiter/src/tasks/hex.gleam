import api/hex as api
import api/hex_repo
import api/signatures
import gleam/bool
import gleam/function_
import gleam/hexpm.{type Package}
import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/result
import gleam/result_
import gleam/time/calendar
import gleam/time/duration
import gleam/time/timestamp.{type Timestamp}
import jupiter/context.{type Context, Context}
import jupiter/data/hex_read.{type HexRead}
import jupiter/data/interfaces.{type Interfaces, Interfaces}
import jupiter/gleam/context as gcontext
import jupiter/loss.{type Error}
import jupiter/postgres/queries
import palabres
import pog
import processes/retrier

const module = "tasks/hex"

type State {
  State(
    page: Int,
    limit: Timestamp,
    newest: Timestamp,
    hex_api_key: String,
    last_logged: Timestamp,
    db: pog.Connection,
  )
}

type WorkMode {
  WorkAsync
  WorkSync
}

pub fn sync_new_gleam_releases(ctx: Context) -> Result(HexRead, Error) {
  palabres.info("Syncing new releases from Hex")
  |> palabres.at(module:, function: "sync_new_gleam_releases")
  |> palabres.log
  use limit <- result.try(queries.get_last_hex_date(ctx.db))
  use latest <- result.try(sync_packages(init_state(ctx, limit)))
  use _ <- function_.tap(queries.upsert_most_recent_hex_timestamp(
    ctx.db,
    latest,
  ))
  palabres.info("Up to date!")
  |> palabres.at(module:, function: "sync_new_gleam_releases")
  |> palabres.log
}

fn init_state(ctx, limit) {
  let Context(hex_api_key:, db:, ..) = ctx
  let last_logged = timestamp.system_time()
  State(page: 1, limit:, newest: limit, hex_api_key:, last_logged:, db:)
}

fn keep_newest_date(package: hexpm.Package, state: State) {
  case timestamp.compare(package.updated_at, state.newest) {
    order.Gt -> package.updated_at
    order.Lt -> state.newest
    order.Eq -> state.newest
  }
}

fn first_timestamp(packages: List(hexpm.Package), state: State) -> Timestamp {
  packages
  |> list.first
  |> result.map(keep_newest_date(_, state))
  |> result.unwrap(state.newest)
}

fn sync_packages(state: State) -> Result(Timestamp, Error) {
  let State(page:, hex_api_key:, ..) = state
  use all_packages <- result.try(api.get_api_packages_page(page, hex_api_key))
  let state = State(..state, newest: first_timestamp(all_packages, state))
  let new_packages = take_fresh_packages(all_packages, state.limit)
  let package_names = list.map(all_packages, fn(p) { p.name })
  palabres.debug("Taking fresh packages")
  |> palabres.list("packages", package_names, palabres.string)
  |> palabres.int("page", page)
  |> palabres.at(module:, function: "sync_packages")
  |> palabres.log
  use state <- result.try({
    use state, package <- list.try_fold(new_packages, state)
    do_sync_package(state, WorkAsync, force: False, package:)
  })
  case list.length(all_packages) == list.length(new_packages) {
    _ if all_packages == [] -> Ok(state.newest)
    False -> Ok(state.newest)
    True -> sync_packages(State(..state, page: state.page + 1))
  }
}

pub fn sync_package(ctx: Context, package: hexpm.Package) {
  State(
    page: -1,
    limit: timestamp.system_time(),
    newest: timestamp.system_time(),
    hex_api_key: ctx.hex_api_key,
    last_logged: timestamp.system_time(),
    db: ctx.db,
  )
  |> do_sync_package(WorkAsync, force: True, package:)
  |> result.replace_error(loss.EmptyError)
  |> result.replace(Nil)
}

fn do_sync_package(
  state: State,
  work_mode work_mode: WorkMode,
  force force_old_release_update: Bool,
  package package: hexpm.Package,
) -> Result(State, Error) {
  let secret = state.hex_api_key
  use releases <- result.try(lookup_gleam_releases(package, secret:))
  use <- bool.lazy_guard(when: list.is_empty(releases), return: fn() {
    let print_deadline = timestamp.add(state.last_logged, duration.seconds(5))
    case timestamp.compare(print_deadline, timestamp.system_time()) {
      order.Eq -> Ok(state)
      order.Gt -> Ok(state)
      order.Lt -> {
        let date = timestamp.to_rfc3339(package.updated_at, calendar.utc_offset)
        palabres.info("Still syncing")
        |> palabres.string("up_to", date)
        |> palabres.at(module:, function: "do_sync_package")
        |> palabres.log
        Ok(State(..state, last_logged: timestamp.system_time()))
      }
    }
  })
  use _ <- result.map({
    insert_package_and_releases(
      package,
      releases,
      state,
      work_mode,
      force_old_release_update,
    )
  })
  State(..state, last_logged: timestamp.system_time())
}

fn extract_interfaces_from_db(
  state: State,
  id: String,
  release: hexpm.Release,
) {
  use r <- result.try(queries.upsert_release(state.db, id, release, None, None))
  case list.first(r.rows) {
    Ok(row) -> Ok(row)
    Error(_) -> {
      palabres.debug("No interfaces in DB")
      |> palabres.string("package_release", release.version)
      |> palabres.at(module:, function: "extract_interfaces_from_db")
      |> palabres.log
      loss.new("No interfaces in DB")
    }
  }
}

fn extract_interfaces_from_hex(
  state: State,
  id: String,
  package: hexpm.Package,
  release: hexpm.Release,
) {
  let content = hex_repo.get_package_infos(package.name, release.version)
  use content <- result.map(content)
  let interface = Some(content.package_interface)
  let gleam_toml = Some(content.gleam_toml)
  queries.upsert_release(state.db, id, release, interface, gleam_toml)
  |> loss.dismiss
  #(content.package, content.toml)
}

fn extract_release_interfaces(
  state: State,
  id: String,
  package: hexpm.Package,
  release: hexpm.Release,
  interfaces: Interfaces,
) {
  use <- result.lazy_or(case interfaces {
    Interfaces(_, Some(interface), Some(toml)) -> {
      use _ <- result_.tap(hex_repo.parse_files(interface, toml))
      palabres.debug("Using interfaces from database")
      |> palabres.string("package_name", package.name)
      |> palabres.string("package_release", release.version)
      |> palabres.at(module:, function: "extract_release_interfaces")
    }
    _ -> loss.new("No release data")
  })
  extract_interfaces_from_hex(state, id, package, release)
}

fn save_retirement_data(
  state: State,
  interfaces: Interfaces,
  package: hexpm.Package,
  release: hexpm.Release,
) -> Result(Nil, Error) {
  case release.retirement {
    None -> Ok(Nil)
    Some(retirement) -> {
      let release = package.name <> " v" <> release.version
      palabres.info("Release is retired.")
      |> palabres.string("release", release)
      |> palabres.nullable("message", retirement.message, palabres.string)
      |> palabres.string("reason", reason_to_string(retirement.reason))
      |> palabres.at(module:, function: "save_retirement_data")
      |> palabres.log
      queries.add_package_retirement(state.db, retirement, interfaces.id)
    }
  }
}

fn reason_to_string(reason: hexpm.RetirementReason) {
  case reason {
    hexpm.OtherReason -> "other"
    hexpm.Invalid -> "invalid"
    hexpm.Security -> "security"
    hexpm.Deprecated -> "deprecated"
    hexpm.Renamed -> "renamed"
  }
}

fn insert_package_and_releases(
  package: hexpm.Package,
  releases: List(hexpm.Release),
  state: State,
  work_async: WorkMode,
  force_old_release_update: Bool,
) -> Result(Nil, Error) {
  let State(hex_api_key: secret, ..) = state
  let versions = list.map(releases, fn(release) { release.version })
  palabres.info("Saving package versions")
  |> palabres.string("package_name", package.name)
  |> palabres.list("package_versions", versions, palabres.string)
  |> palabres.at(module:, function: "insert_package_and_releases")
  |> palabres.log
  use id <- result.try(queries.upsert_package(state.db, package))

  use owners <- result.try(api.get_package_owners(package.name, secret:))
  let owners_ = list.map(owners, fn(owner) { owner.username })
  palabres.debug("Saving owners for package")
  |> palabres.string("package_name", package.name)
  |> palabres.list("package_owners", owners_, palabres.string)
  |> palabres.at(module:, function: "insert_package_and_releases")
  |> palabres.log
  use _ <- result.try(queries.sync_package_owners(state.db, id, owners))

  palabres.debug("Saving releases for package")
  |> palabres.string("package_name", package.name)
  |> palabres.at(module:, function: "insert_package_and_releases")
  |> palabres.log
  use release <- list.try_each(releases)
  // When release does not exists, il will continue.
  // Forcing the update will send an error no matter what to continue.
  use <- result.lazy_or({
    let lookup = queries.lookup_release(state.db, id, release)
    case lookup, force_old_release_update {
      Ok(_), True -> loss.empty()
      Ok(_), False -> Ok(Nil)
      Error(error), True -> Error(error)
      Error(error), False -> Error(error)
    }
  })

  palabres.debug("Handling package release")
  |> palabres.string("package_name", package.name)
  |> palabres.string("package_release", release.version)
  |> palabres.at(module:, function: "insert_package_and_releases")
  |> palabres.log
  use interfaces <- result.try(extract_interfaces_from_db(state, id, release))
  use _ <- result.try(save_retirement_data(state, interfaces, package, release))
  case work_async {
    WorkSync -> extract_package(state, id, release, package, interfaces, False)
    WorkAsync -> {
      let slug = package.name <> " v" <> release.version
      retrier.retry(fn(iterations) {
        let ignore = iterations == 0
        palabres.notice("Trying extracting package")
        |> palabres.int("iterations", iterations)
        |> palabres.string("slug", slug)
        |> palabres.at(module:, function: "insert_package_and_releases")
        |> palabres.log
        extract_package(state, id, release, package, interfaces, ignore)
      })
      |> result.replace(Nil)
      |> result.map_error(loss.ActorError)
    }
  }
}

fn extract_package(
  state: State,
  id: String,
  release: hexpm.Release,
  package: hexpm.Package,
  interfaces: Interfaces,
  ignore_parameters_errors: Bool,
) -> Result(Nil, Error) {
  use #(package_interface, gleam_toml) <- result.try({
    extract_release_interfaces(state, id, package, release, interfaces)
  })
  gcontext.Context(
    db: state.db,
    package_interface:,
    gleam_toml:,
    ignore_parameters_errors:,
  )
  |> signatures.extract_signatures
  |> result.replace(Nil)
  |> result_.tap(fn(_content) {
    let release = package.name <> " v" <> release.version
    palabres.notice("Finished extracting release!")
    |> palabres.string("release", release)
    |> palabres.at(module:, function: "extract_package")
    |> palabres.log
  })
}

fn lookup_gleam_releases(
  package: hexpm.Package,
  secret hex_api_key: String,
) -> Result(List(hexpm.Release), Error) {
  package.releases
  |> list.try_map(api.lookup_release(_, hex_api_key))
  |> result.map(fn(releases) {
    use release <- list.filter(releases)
    list.contains(release.meta.build_tools, "gleam")
  })
}

pub fn take_fresh_packages(packages: List(Package), limit: Timestamp) {
  use package <- list.take_while(packages)
  timestamp.compare(limit, package.updated_at) == order.Lt
}
