import api/hex as api
import api/hex_repo
import api/signatures
import backend/config.{type Config}
import backend/data/hex_read.{type HexRead}
import backend/error.{type Error}
import backend/gleam/context
import backend/postgres/postgres
import backend/postgres/queries
import birl.{type Time}
import birl/duration
import gleam/hexpm.{type Package}
import gleam/int
import gleam/list
import gleam/option
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
  )
}

pub fn sync_new_gleam_releases(
  cnf: Config,
  children: supervisor.Children(Nil),
) -> Result(HexRead, Error) {
  let ctx = postgres.connect(cnf)
  wisp.log_info("Syncing new releases from Hex")
  use limit <- result.try(queries.get_last_hex_date(ctx.db))
  use latest <- result.try(sync_packages(
    State(
      page: 1,
      limit: limit,
      newest: limit,
      hex_api_key: cnf.hex_api_key,
      last_logged: birl.now(),
      db: ctx.db,
    ),
    children,
  ))
  let latest = queries.upsert_most_recent_hex_timestamp(ctx.db, latest)
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
    sync_package(children),
  ))
  case list.length(all_packages) == list.length(new_packages) {
    _ if all_packages == [] -> Ok(state.newest)
    False -> Ok(state.newest)
    True -> sync_packages(State(..state, page: state.page + 1), children)
  }
}

fn sync_package(children: supervisor.Children(Nil)) {
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
        ))
        State(..state, last_logged: birl.now())
      }
    }
  }
}

fn log_retirement_data(release: String, retirement: hexpm.ReleaseRetirement) {
  wisp.log_info("Release " <> release <> " is retired. Skipping.")
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

fn insert_package_and_releases(
  package: hexpm.Package,
  releases: List(hexpm.Release),
  state: State,
  children: supervisor.Children(Nil),
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
  list.try_each(releases, fn(r) {
    let release = package.name <> " v" <> r.version
    use _ <- result.map(queries.upsert_release(state.db, id, r))
    case r.retirement {
      option.Some(retirement) -> log_retirement_data(release, retirement)
      option.None -> {
        supervisor.add(children, {
          use _ <- supervisor.worker()
          use iterations <- retrier.retry()
          let it = int.to_string(iterations)
          wisp.log_notice("Trying iteration " <> it <> " for " <> release)
          let infos = hex_repo.get_package_infos(package.name, r.version)
          use #(package, gleam_toml) <- result.try(infos)
          context.Context(state.db, package, gleam_toml, iterations == 0)
          |> signatures.extract_signatures()
          |> result.map(fn(content) {
            wisp.log_notice("Finished extracting " <> release <> "!")
            content
          })
        })
        Nil
      }
    }
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
