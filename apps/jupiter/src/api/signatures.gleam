import data/ir
import data/ir/from_gleam
import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/package_interface
import gleam/result
import jupiter/gleam/context.{type Context}
import jupiter/gleam/generate/metadata
import jupiter/gleam/generate/sources
import jupiter/postgres/queries
import palabres

const module_name = "api/signatures"

/// Alpha-rename a lowered declaration, then derive the columns that hang off the
/// IR: the encoded `signature_ir` and its phase-1 fingerprint (`arity`,
/// `type_names`). All three come from the one canonical declaration.
pub fn structured(
  declaration: ir.Declaration,
) -> #(json.Json, Int, List(String)) {
  let declaration = ir.alpha_rename_declaration(declaration)
  let #(arity, type_names) = ir.fingerprint(declaration)
  #(ir.encode(declaration), arity, type_names)
}

fn add_gleam_constraint(ctx: Context, release_id: String) {
  case ctx.package_interface.gleam_version_constraint {
    Some(c) -> queries.add_package_gleam_constraint(ctx.db, c, release_id)
    None -> Ok(Nil)
  }
}

fn upsert_type_definitions(ctx: Context, module: context.Module) {
  let name = context.qualified_name(ctx, module)
  palabres.debug("Extracting type definitions")
  |> palabres.string("name", name)
  |> palabres.string("module_id", module.id)
  |> palabres.string("release_id", module.release_id)
  |> palabres.at(module: module_name, function: "upsert_type_definitions")
  |> palabres.log
  let all_types = dict.to_list(module.module.types)
  result.all({
    let kind = queries.TypeDefinition
    use #(name, def) <- list.map(all_types)
    // Insert type upfront to achieve recursive types.
    let _ =
      queries.upsert_package_type_fun_signature(
        db: ctx.db,
        kind:,
        name:,
        documentation: option.None,
        metadata: json.null(),
        signature: "",
        signature_ir: json.null(),
        arity: 0,
        type_names: [],
        module_id: module.id,
        deprecation: option.None,
        implementations: None,
      )
    let #(signature_ir, arity, type_names) =
      structured(from_gleam.type_definition(def))
    queries.upsert_package_type_fun_signature(
      db: ctx.db,
      kind:,
      name:,
      documentation: def.documentation,
      metadata: metadata.generate(def.deprecation, None),
      signature: sources.type_definition_to_string(name, def),
      signature_ir:,
      arity:,
      type_names:,
      module_id: module.id,
      deprecation: def.deprecation,
      implementations: None,
    )
  })
}

fn upsert_type_aliases(ctx: Context, module: context.Module) {
  let name = context.qualified_name(ctx, module)
  palabres.debug("Extracting type aliases")
  |> palabres.string("name", name)
  |> palabres.string("module_id", module.id)
  |> palabres.string("release_id", module.release_id)
  |> palabres.at(module: module_name, function: "upsert_type_aliases")
  |> palabres.log
  let all_types = dict.to_list(module.module.type_aliases)
  result.all({
    let kind = queries.TypeAlias
    use #(name, alias) <- list.map(all_types)
    // Insert type upfront to achieve recursive types.
    let _ =
      queries.upsert_package_type_fun_signature(
        db: ctx.db,
        kind:,
        name:,
        documentation: option.None,
        metadata: json.null(),
        signature: "",
        signature_ir: json.null(),
        arity: 0,
        type_names: [],
        module_id: module.id,
        deprecation: option.None,
        implementations: None,
      )
    let #(signature_ir, arity, type_names) =
      structured(from_gleam.type_alias(alias))
    queries.upsert_package_type_fun_signature(
      db: ctx.db,
      name:,
      kind:,
      documentation: alias.documentation,
      metadata: metadata.generate(alias.deprecation, None),
      signature: sources.type_alias_to_string(name, alias),
      signature_ir:,
      arity:,
      type_names:,
      module_id: module.id,
      deprecation: alias.deprecation,
      implementations: None,
    )
  })
}

fn upsert_constants(ctx: Context, module: context.Module) {
  let name = context.qualified_name(ctx, module)
  palabres.debug("Extracting constants")
  |> palabres.string("name", name)
  |> palabres.string("module_id", module.id)
  |> palabres.string("release_id", module.release_id)
  |> palabres.at(module: module_name, function: "upsert_constants")
  |> palabres.log
  let all_constants = dict.to_list(module.module.constants)
  result.all({
    use #(name, constant) <- list.map(all_constants)
    let #(signature_ir, arity, type_names) =
      structured(from_gleam.constant(constant))
    queries.upsert_package_type_fun_signature(
      db: ctx.db,
      name:,
      kind: queries.Constant,
      documentation: constant.documentation,
      metadata: Some(constant.implementations)
        |> metadata.generate(constant.deprecation, _),
      signature: sources.constant_to_string(name, constant),
      signature_ir:,
      arity:,
      type_names:,
      module_id: module.id,
      deprecation: constant.deprecation,
      implementations: Some(constant.implementations),
    )
  })
}

fn upsert_functions(ctx: Context, module: context.Module) {
  let name = context.qualified_name(ctx, module)
  palabres.debug("Extracting functions")
  |> palabres.string("name", name)
  |> palabres.string("module_id", module.id)
  |> palabres.string("release_id", module.release_id)
  |> palabres.at(module: module_name, function: "upsert_functions")
  |> palabres.log
  let all_functions = dict.to_list(module.module.functions)
  result.all({
    use #(name, function) <- list.map(all_functions)
    let signature = sources.function_to_string(name, function)
    let #(signature_ir, arity, type_names) =
      structured(from_gleam.function(function))
    let content =
      queries.upsert_package_type_fun_signature(
        db: ctx.db,
        name:,
        kind: queries.Function,
        documentation: function.documentation,
        metadata: Some(function.implementations)
          |> metadata.generate(function.deprecation, _),
        signature:,
        signature_ir:,
        arity:,
        type_names:,
        module_id: module.id,
        deprecation: function.deprecation,
        implementations: Some(function.implementations),
      )
    content
  })
}

fn extract_module_signatures(
  ctx: Context,
  release_id: String,
  module: #(String, package_interface.Module),
) {
  let module = context.Module(module.1, "-1", module.0, release_id)
  let name = context.qualified_name(ctx, module)
  palabres.debug("Extracting signatures")
  |> palabres.string("name", name)
  |> palabres.string("module_id", module.id)
  |> palabres.string("release_id", module.release_id)
  |> palabres.at(module: module_name, function: "extract_module_signatures")
  |> palabres.log
  use module_id <- result.try(queries.upsert_package_module(ctx.db, module))
  let module = context.Module(..module, id: module_id)
  use _ <- result.try(upsert_type_definitions(ctx, module))
  use _ <- result.try(upsert_type_aliases(ctx, module))
  use _ <- result.try(upsert_constants(ctx, module))
  case upsert_functions(ctx, module) {
    Error(err) -> Error(err)
    Ok(content) -> {
      palabres.debug("Extracting signatures finished")
      |> palabres.string("name", name)
      |> palabres.string("module_id", module.id)
      |> palabres.string("release_id", module.release_id)
      |> palabres.at(module: module_name, function: "extract_module_signatures")
      |> palabres.log
      Ok(content)
    }
  }
}

pub fn extract_signatures(ctx: Context) {
  let package = ctx.package_interface
  let package_slug = package.name <> "@" <> package.version
  palabres.debug("Extracting signatures")
  |> palabres.string("package_slug", package_slug)
  |> palabres.at(module: module_name, function: "extract_signatures")
  |> palabres.log
  let res = queries.get_package_release_ids(ctx.db, ctx.package_interface)
  use #(_pid, release_id) <- result.try(res)
  use _ <- result.try(add_gleam_constraint(ctx, release_id))
  package.modules
  |> dict.to_list
  |> list.map(extract_module_signatures(ctx, release_id, _))
  |> result.all
}
