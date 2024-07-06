import api/hex as api
import api/hex_repo
import api/signatures
import backend/config.{type Context}
import backend/data/hex_read.{type HexRead}
import backend/error.{type Error}
import backend/gleam/context
import backend/gleam/type_search/state as type_search
import backend/postgres/queries
import birl.{type Time}
import birl/duration
import gleam/erlang/process.{type Subject}
import gleam/function
import gleam/hexpm.{type Package}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/otp/supervisor
import gleam/pgo
import gleam/result
import gleam/string
import retrier
import wisp

type State {
  State(
    page: Int,
    limit: Time,
    newest: Time,
    hex_api_key: String,
    last_logged: Time,
    db: pgo.Connection,
    type_search_subject: Option(Subject(type_search.Msg)),
  )
}

pub fn sync_new_gleam_releases(
  ctx: Context,
  children: supervisor.Children(Nil),
) -> Result(HexRead, Error) {
  wisp.log_info("Syncing new releases from Hex")
  use limit <- result.try(queries.get_last_hex_date(ctx.db))
  use latest <- result.try(sync_packages(
    State(
      page: 1,
      limit: limit,
      newest: limit,
      hex_api_key: ctx.hex_api_key,
      last_logged: birl.now(),
      db: ctx.db,
      type_search_subject: ctx.type_search_subject,
    ),
    children,
  ))
  let latest = queries.upsert_most_recent_hex_timestamp(ctx.db, latest)
  wisp.log_info("")
  wisp.log_info("Up to date!")
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

fn sync_packages(
  state: State,
  children: supervisor.Children(Nil),
) -> Result(Time, Error) {
  let page = state.page
  let api_key = state.hex_api_key
  use all_packages <- result.try(api.get_api_packages_page(page, api_key))
  let state = State(..state, newest: first_timestamp(all_packages, state))
  let new_packages = take_fresh_packages(all_packages, state.limit)
  use state <- result.try(list.try_fold(
    new_packages,
    state,
    do_sync_package(Some(children), force: False),
  ))
  case list.length(all_packages) == list.length(new_packages) {
    _ if all_packages == [] -> Ok(state.newest)
    False -> Ok(state.newest)
    True -> sync_packages(State(..state, page: state.page + 1), children)
  }
}

fn do_sync_package(
  children: Option(supervisor.Children(Nil)),
  force force_old_release_update: Bool,
) {
  fn(state: State, package: hexpm.Package) -> Result(State, Error) {
    let secret = state.hex_api_key
    use releases <- result.try(lookup_gleam_releases(package, secret: secret))
    case releases {
      [] -> Ok(log_if_needed(state, package.updated_at))
      _ -> {
        use _ <- result.map(insert_package_and_releases(
          package,
          releases,
          state,
          children,
          force_old_release_update,
        ))
        State(..state, last_logged: birl.now())
      }
    }
  }
}

pub fn sync_package(ctx: Context, package: hexpm.Package) {
  do_sync_package(None, force: True)(
    State(
      page: -1,
      limit: birl.now(),
      newest: birl.now(),
      hex_api_key: ctx.hex_api_key,
      last_logged: birl.now(),
      db: ctx.db,
      type_search_subject: ctx.type_search_subject,
    ),
    package,
  )
  |> result.replace_error(error.EmptyError)
  |> result.replace(Nil)
}

fn log_retirement_data(release: String, retirement: hexpm.ReleaseRetirement) {
  wisp.log_info("Release " <> release <> " is retired.")
  case retirement.message {
    option.Some(m) -> wisp.log_debug("  Retired because " <> m)
    option.None -> Nil
  }
  case retirement.reason {
    hexpm.OtherReason -> wisp.log_debug("  Retired for an other reason")
    hexpm.Invalid -> wisp.log_debug("  Retired because it was invalid")
    hexpm.Security -> wisp.log_debug("  Retired for security reasons")
    hexpm.Deprecated -> wisp.log_debug("  Retired because it's deprecated")
    hexpm.Renamed -> wisp.log_debug("  Retired because it's renamed")
  }
}

fn extract_release_interfaces_from_db(
  state: State,
  id: Int,
  release: hexpm.Release,
) {
  use _ <- result.try_recover(queries.lookup_release(state.db, id, release))
  use r <- result.try(queries.upsert_release(state.db, id, release, None, None))
  r.rows
  |> list.first()
  |> result.replace_error(error.UnknownError(""))
}

fn extract_release_interfaces_from_hex(
  state: State,
  id: Int,
  package: hexpm.Package,
  release: hexpm.Release,
) {
  use data <- result.map({
    hex_repo.get_package_infos(package.name, release.version)
  })
  let interface = Some(data.2)
  let toml = Some(data.3)
  let _ = queries.upsert_release(state.db, id, release, interface, toml)
  #(data.0, data.1)
}

fn extract_release_interfaces(
  state: State,
  id: Int,
  package: hexpm.Package,
  release: hexpm.Release,
  interfaces: #(Int, Option(String), Option(String)),
) {
  use _ <- result.try_recover(case interfaces {
    #(_, Some(interface), Some(toml)) -> {
      use content <- result.map(hex_repo.parse_files(interface, toml))
      wisp.log_debug("Using interfaces from database")
      content
    }
    _ -> Error(error.UnknownError("No release data"))
  })
  extract_release_interfaces_from_hex(state, id, package, release)
}

fn save_retirement_data(
  state: State,
  release_id: Int,
  package: hexpm.Package,
  release: hexpm.Release,
) {
  case release.retirement {
    option.None -> Nil
    option.Some(retirement) -> {
      let release = package.name <> " v" <> release.version
      log_retirement_data(release, retirement)
      let _ =
        queries.add_package_gleam_retirement(state.db, retirement, release_id)
      Nil
    }
  }
}

fn insert_package_and_releases(
  package: hexpm.Package,
  releases: List(hexpm.Release),
  state: State,
  children: Option(supervisor.Children(Nil)),
  force_old_release_update: Bool,
) {
  let secret = state.hex_api_key
  let versions =
    releases
    |> list.map(fn(release) { release.version })
    |> string.join(", v")
  wisp.log_info("Saving " <> package.name <> " v" <> versions)
  use id <- result.try(queries.upsert_package(state.db, package))

  wisp.log_debug("Saving owners for " <> package.name)
  use owners <- result.try(api.get_package_owners(package.name, secret: secret))
  use _ <- result.try(queries.sync_package_owners(state.db, id, owners))

  wisp.log_debug("Saving releases for " <> package.name)
  use r <- list.try_each(releases)
  let release = package.name <> " v" <> r.version
  // When release does not exists, il will continue.
  // Forcing the update will send an error no matter what to continue.
  use _ <- result.try_recover({
    queries.lookup_release(state.db, id, r)
    |> result.replace(Nil)
    |> case force_old_release_update {
      True -> result.try(_, fn(_) { Error(error.EmptyError) })
      False -> function.identity
    }
  })
  wisp.log_debug("Handling release " <> r.version)
  use interfaces <- result.map(extract_release_interfaces_from_db(state, id, r))
  save_retirement_data(state, interfaces.0, package, r)
  let _ = case children {
    None -> {
      let _ = do_extract_package(state, id, r, package, interfaces, False)
      Nil
    }
    Some(children) -> {
      supervisor.add(children, {
        use _ <- supervisor.worker()
        use iterations <- retrier.retry()
        let it = int.to_string(iterations)
        wisp.log_notice("Trying iteration " <> it <> " for " <> release)
        do_extract_package(state, id, r, package, interfaces, iterations == 0)
      })
      Nil
    }
  }
  Nil
}

fn do_extract_package(
  state: State,
  id: Int,
  release: hexpm.Release,
  package: hexpm.Package,
  interfaces: #(Int, Option(String), Option(String)),
  ignore_errors: Bool,
) {
  use #(package, gleam_toml) <- result.try({
    extract_release_interfaces(state, id, package, release, interfaces)
  })
  context.Context(
    state.db,
    package,
    gleam_toml,
    ignore_errors,
    state.type_search_subject,
  )
  |> signatures.extract_signatures()
  |> result.map(fn(content) {
    let release = package.name <> " v" <> release.version
    wisp.log_notice("Finished extracting " <> release <> "!")
    content
  })
}

fn lookup_gleam_releases(
  package: hexpm.Package,
  secret hex_api_key: String,
) -> Result(List(hexpm.Release), Error) {
  let lookup =
    list.try_map(package.releases, api.lookup_release(_, hex_api_key))
  use releases <- result.map(lookup)
  list.filter(releases, fn(r) { list.contains(r.meta.build_tools, "gleam") })
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
