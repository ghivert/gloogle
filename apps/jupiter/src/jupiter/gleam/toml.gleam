import gleam/bool
import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import jupiter/gleam/context.{type Context}
import jupiter/loss
import tom

/// A declared dependency of a release, as `gleam.toml` lists it.
pub type Dependency {
  Dependency(name: String, requirement: String, kind: DependencyKind)
}

pub type DependencyKind {
  Runtime
  Development
}

/// The `[dependencies]` and `[dev-dependencies]` of a parsed `gleam.toml`, in
/// that order. Only string requirements count (`"~> 4.1"`, `">= 1.0.0 and <
/// 2.0.0"`): a path or git dependency is not on Hex, so it has no release to
/// link to.
pub fn dependencies(toml: Dict(String, tom.Toml)) -> List(Dependency) {
  list.flatten([
    table(toml, "dependencies", Runtime),
    table(toml, "dev-dependencies", Development),
  ])
}

fn table(
  toml: Dict(String, tom.Toml),
  key: String,
  kind: DependencyKind,
) -> List(Dependency) {
  tom.get_table(toml, [key])
  |> result.unwrap(dict.new())
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    case entry {
      #(name, tom.String(requirement)) ->
        Ok(Dependency(name:, requirement:, kind:))
      _ -> Error(Nil)
    }
  })
}

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
  |> result.map_error(loss.TomlGetError)
}
