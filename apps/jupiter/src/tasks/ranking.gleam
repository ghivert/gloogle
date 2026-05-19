import function
import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/option
import gleam/result
import jupiter/context.{type Context}
import jupiter/error.{type Error}
import jupiter/postgres/queries
import palabres
import tom

const module = "tasks/ranking"

type Usages =
  Dict(String, Int)

pub fn compute_ranking(ctx: Context) -> Result(Nil, Error) {
  palabres.info("Syncing package ranks")
  |> palabres.at(module:, function: "compute_ranking")
  |> palabres.log
  use _ <- function.tap(compute_and_save_rankings(ctx))
  palabres.info("Syncing package ranks finished!")
  |> palabres.at(module:, function: "compute_ranking")
  |> palabres.log
}

fn compute_and_save_rankings(ctx: Context) {
  compute_packages_ranking(ctx)
  |> result.try(save_packages_rank(ctx, _))
  |> result.map_error(error.debug_log)
  |> result.replace(Nil)
}

fn compute_packages_ranking(ctx: Context) {
  use usages, gleam_toml <- do_compute_packages_ranking(ctx, 0, dict.new())
  tom.parse(gleam_toml)
  |> result.map_error(error.ParseTomlError)
  |> result.map(add_dependencies(from: _, in: usages))
  |> result.unwrap(usages)
}

fn do_compute_packages_ranking(
  ctx: Context,
  offset: Int,
  usages: Usages,
  do work: fn(Usages, String) -> Usages,
) -> Result(Usages, Error) {
  use tomls <- result.try(queries.select_gleam_toml(ctx.db, offset))
  use <- bool.guard(when: list.is_empty(tomls), return: Ok(usages))
  tomls
  |> list.fold(usages, work)
  |> do_compute_packages_ranking(ctx, offset + 100, _, do: work)
}

fn get_dependencies(toml: Dict(String, tom.Toml)) {
  let unwrap = result.lazy_unwrap(_, dict.new)
  let deps = tom.get_table(toml, ["dependencies"]) |> unwrap
  let dev_deps = tom.get_table(toml, ["dev-dependencies"]) |> unwrap
  dict.merge(deps, dev_deps)
  |> dict.keys
}

fn add_dependencies(in usages: Usages, from toml: Dict(String, tom.Toml)) {
  let dependencies = get_dependencies(toml)
  use usages, dep <- list.fold(from: usages, over: dependencies)
  use value <- dict.upsert(usages, dep)
  option.unwrap(value, 0) + 1
}

fn save_packages_rank(ctx: Context, usages: Usages) {
  dict.to_list(usages)
  |> list.map(save_package_rank(ctx, _))
  |> result.all
}

fn save_package_rank(ctx: Context, usage: #(String, Int)) {
  let #(name, rank) = usage
  queries.update_package_rank(ctx.db, name, rank)
  |> result.replace(Nil)
}
