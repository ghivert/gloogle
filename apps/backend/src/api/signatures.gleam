import backend/gleam/context.{type Context}
import backend/gleam/generate/metadata
import backend/gleam/generate/sources.{
  constant_to_string, function_to_string, type_alias_to_string,
  type_definition_to_string,
}
import backend/gleam/generate/types.{
  constant_to_json, function_to_json, type_alias_to_json,
  type_definition_to_json,
}
import backend/postgres/queries
import gleam/bool
import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/package_interface
import gleam/result
import wisp

fn add_gleam_constraint(ctx: Context, release_id: Int) {
  case ctx.package_interface.gleam_version_constraint {
    Some(c) -> queries.add_package_gleam_constraint(ctx.db, c, release_id)
    None -> Ok(Nil)
  }
}

fn upsert_type_definitions(ctx: Context, module: context.Module) {
  let name = context.qualified_name(ctx, module)
  wisp.log_debug("Extracting " <> name <> " type definitions")
  let all_types = dict.to_list(module.module.types)
  result.all({
    use #(type_name, type_def) <- list.map(all_types)
    // Insert type upfront to achieve recursive types.
    let _ =
      queries.upsert_package_type_fun_signature(
        db: ctx.db,
        kind: queries.TypeDefinition,
        name: type_name,
        documentation: option.None,
        metadata: json.null(),
        signature: "",
        json_signature: json.null(),
        parameters: [],
        module_id: module.id,
        deprecation: option.None,
        implementations: None,
      )
    use gen <- result.try(type_definition_to_json(ctx, type_name, type_def))
    queries.upsert_package_type_fun_signature(
      db: ctx.db,
      kind: queries.TypeDefinition,
      name: type_name,
      documentation: type_def.documentation,
      metadata: metadata.generate(type_def.deprecation, None),
      signature: type_definition_to_string(type_name, type_def),
      json_signature: gen.0,
      parameters: gen.1,
      module_id: module.id,
      deprecation: type_def.deprecation,
      implementations: None,
    )
  })
}

fn upsert_type_aliases(ctx: Context, module: context.Module) {
  let name = context.qualified_name(ctx, module)
  wisp.log_debug("Extracting " <> name <> " type aliases")
  let all_types = dict.to_list(module.module.type_aliases)
  result.all({
    use #(type_name, type_alias) <- list.map(all_types)
    // Insert type upfront to achieve recursive types.
    let _ =
      queries.upsert_package_type_fun_signature(
        db: ctx.db,
        kind: queries.TypeAlias,
        name: type_name,
        documentation: option.None,
        metadata: json.null(),
        signature: "",
        json_signature: json.null(),
        parameters: [],
        module_id: module.id,
        deprecation: option.None,
        implementations: None,
      )
    use gen <- result.try(type_alias_to_json(ctx, type_name, type_alias))
    queries.upsert_package_type_fun_signature(
      db: ctx.db,
      name: type_name,
      kind: queries.TypeAlias,
      documentation: type_alias.documentation,
      metadata: metadata.generate(type_alias.deprecation, None),
      signature: type_alias_to_string(type_name, type_alias),
      json_signature: gen.0,
      parameters: gen.1,
      module_id: module.id,
      deprecation: type_alias.deprecation,
      implementations: None,
    )
  })
}

fn upsert_constants(ctx: Context, module: context.Module) {
  let name = context.qualified_name(ctx, module)
  wisp.log_debug("Extracting " <> name <> " constants")
  let all_constants = dict.to_list(module.module.constants)
  result.all({
    use #(constant_name, constant) <- list.map(all_constants)
    use gen <- result.try(constant_to_json(ctx, constant_name, constant))
    queries.upsert_package_type_fun_signature(
      db: ctx.db,
      name: constant_name,
      kind: queries.Constant,
      documentation: constant.documentation,
      metadata: Some(constant.implementations)
        |> metadata.generate(constant.deprecation, _),
      signature: constant_to_string(constant_name, constant),
      json_signature: gen.0,
      parameters: gen.1,
      module_id: module.id,
      deprecation: constant.deprecation,
      implementations: Some(constant.implementations),
    )
  })
}

fn upsert_functions(ctx: Context, module: context.Module) {
  let name = context.qualified_name(ctx, module)
  wisp.log_debug("Extracting " <> name <> " functions")
  let all_functions = dict.to_list(module.module.functions)
  result.all({
    use #(function_name, function) <- list.map(all_functions)
    use gen <- result.try(function_to_json(ctx, function_name, function))
    queries.upsert_package_type_fun_signature(
      db: ctx.db,
      name: function_name,
      kind: queries.Function,
      documentation: function.documentation,
      metadata: Some(function.implementations)
        |> metadata.generate(function.deprecation, _),
      signature: function_to_string(function_name, function),
      json_signature: gen.0,
      parameters: gen.1,
      module_id: module.id,
      deprecation: function.deprecation,
      implementations: Some(function.implementations),
    )
  })
}

fn extract_module_signatures(
  ctx: Context,
  release_id: Int,
  module: #(String, package_interface.Module),
) {
  let module = context.Module(module.1, -1, module.0, release_id)
  let name = context.qualified_name(ctx, module)
  wisp.log_debug("Extracting " <> name <> " signatures")
  use module_id <- result.try(queries.upsert_package_module(ctx.db, module))
  let module = context.Module(..module, id: module_id)
  use _ <- result.try(upsert_type_definitions(ctx, module))
  use _ <- result.try(upsert_type_aliases(ctx, module))
  use _ <- result.try(upsert_constants(ctx, module))
  let res = upsert_functions(ctx, module)
  use <- bool.guard(when: result.is_error(res), return: res)
  wisp.log_debug("Extracting " <> name <> " finished")
  res
}

pub fn extract_signatures(ctx: Context) {
  let package = ctx.package_interface
  let package_slug = package.name <> "@" <> package.version
  wisp.log_debug("Extracting signatures for " <> package_slug)
  let res = queries.get_package_release_ids(ctx.db, ctx.package_interface)
  use #(_pid, release_id) <- result.try(res)
  use _ <- result.try(add_gleam_constraint(ctx, release_id))
  package.modules
  |> dict.to_list()
  |> list.map(extract_module_signatures(ctx, release_id, _))
  |> result.all()
}
