import backend/gleam/type_search/msg as type_search
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}
import gleam/package_interface
import pog
import tom

pub type Context {
  Context(
    db: pog.Connection,
    package_interface: package_interface.Package,
    gleam_toml: Dict(String, tom.Toml),
    /// Allow to bypass parameters relations if activated.
    /// This allows to ignore internals for example.
    ignore_parameters_errors: Bool,
    type_search_subject: Option(Subject(type_search.Msg)),
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
