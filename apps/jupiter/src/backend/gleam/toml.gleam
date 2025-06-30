import backend/error
import backend/gleam/context.{type Context}
import gleam/bool
import gleam/result
import tom

fn is_dependency(ctx: Context, package_name: String) {
  use name <- result.map(tom.get_string(ctx.gleam_toml, ["name"]))
  name != package_name
}

pub fn extract_dep_version(ctx: Context, package_name: String) {
  tom.get_string(ctx.gleam_toml, ["dependencies", package_name])
}

pub fn extract_dev_dep_version(ctx: Context, package_name: String) {
  tom.get_string(ctx.gleam_toml, ["dev-dependencies", package_name])
}

fn extract_package_version(ctx: Context, package_name: String) {
  use is_dep <- result.try(is_dependency(ctx, package_name))
  use <- bool.guard(when: is_dep, return: Error(tom.NotFound([])))
  tom.get_string(ctx.gleam_toml, ["version"])
}

pub fn find_package_requirement(ctx: Context, package_name: String) {
  extract_package_version(ctx, package_name)
  |> result.try_recover(fn(_) { extract_dep_version(ctx, package_name) })
  |> result.try_recover(fn(_) { extract_dev_dep_version(ctx, package_name) })
  |> result.map_error(error.GetTomlError)
}
