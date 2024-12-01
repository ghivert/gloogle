import backend/context.{type Context}
import backend/error.{type Error}
import backend/postgres/queries
import gleam/dict.{type Dict}
import gleam/function
import gleam/list
import gleam/option
import gleam/result
import tom
import wisp

type Usages =
  Dict(String, Int)

pub fn compute_ranking(ctx: Context) -> Result(Nil, Error) {
  wisp.log_info("Syncing package ranks")
  loop(ctx, 0, dict.new(), do: fn(usages, gleam_toml) {
    tom.parse(gleam_toml)
    |> result.map_error(error.ParseTomlError)
    |> result.map(add_dependencies(from: _, in: usages))
    |> result.unwrap(usages)
  })
  |> result.try(save_packages_rank(ctx, _))
  |> result.map_error(error.debug_log)
  |> result.replace(Nil)
  |> function.tap(fn(_) { wisp.log_info("Syncing package ranks finished!") })
}

fn loop(
  ctx: Context,
  offset: Int,
  usages: Usages,
  do work: fn(Usages, String) -> Usages,
) -> Result(Usages, Error) {
  use tomls <- result.try(queries.select_gleam_toml(ctx.db, offset))
  case list.is_empty(tomls) {
    True -> Ok(usages)
    False ->
      tomls
      |> list.fold(usages, work)
      |> loop(ctx, offset + 100, _, do: work)
  }
}

fn get_dependencies(toml: Dict(String, tom.Toml)) {
  dict.merge(
    result.unwrap(tom.get_table(toml, ["dependencies"]), dict.new()),
    result.unwrap(tom.get_table(toml, ["dev-dependencies"]), dict.new()),
  )
  |> dict.keys()
}

fn add_dependencies(in usages: Usages, from toml: Dict(String, tom.Toml)) {
  use usages, dep <- list.fold(from: usages, over: get_dependencies(toml))
  dict.upsert(usages, dep, fn(value) { option.unwrap(value, 0) + 1 })
}

fn save_packages_rank(ctx: Context, usages: Usages) {
  dict.to_list(usages)
  |> list.map(save_package_rank(ctx, _))
  |> result.all()
}

fn save_package_rank(ctx: Context, usage: #(String, Int)) {
  let #(package_name, rank) = usage
  queries.update_package_rank(ctx.db, package_name, rank)
  |> result.replace(Nil)
}
