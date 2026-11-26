import gleam/dict.{type Dict}
import gleam/package_interface
import pog
import tom

pub type Context {
  Context(
    db: pog.Connection,
    package_interface: package_interface.Package,
    gleam_toml: Dict(String, tom.Toml),
  )
}

pub type Module {
  Module(
    module: package_interface.Module,
    id: String,
    name: String,
    release_id: String,
  )
}

pub fn qualified_name(ctx: Context, module: Module) {
  let package = ctx.package_interface
  let module_slug = module.name <> "@" <> package.version
  package.name <> "/" <> module_slug
}
