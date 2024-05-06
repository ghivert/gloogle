import gleam/dict.{type Dict}
import gleam/package_interface
import gleam/pgo
import tom

pub type Context {
  Context(
    db: pgo.Connection,
    package_interface: package_interface.Package,
    gleam_toml: Dict(String, tom.Toml),
    /// Allow to bypass parameters relations if activated.
    /// This allows to ignore internals for example.
    ignore_parameters_errors: Bool,
  )
}

pub type Module {
  Module(
    module: package_interface.Module,
    id: Int,
    name: String,
    release_id: Int,
  )
}

pub fn qualified_name(ctx: Context, module: Module) {
  let package = ctx.package_interface
  let module_slug = module.name <> "@" <> package.version
  package.name <> "/" <> module_slug
}
