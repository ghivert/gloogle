//// Signature IR — the shared, language-agnostic representation of a type that
//// `type_search` matches on. See `signature-ir.md` for the full design.
////
//// A searchable signature is a `Type` (usually an `Fn`). Language front-ends
//// lower their surface syntax into this IR; the canonical notation parser/printer
//// round-trips it. Two invariants:
////   - type variables are alpha-renamed to indices (`Var(Int)`), the source name
////     is dropped;
////   - a method is a receiver-first `Fn` with `method: True`.

import decrypt
import gleam/dict.{type Dict}
import gleam/dynamic/decode
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Type {
  /// `is_method: True` ⇒ the first parameter is the receiver.
  Fn(params: List(Param), return: Type, is_method: Bool)
  /// Nominal type applied to zero or more arguments: `Int`, `List<a>`, `Result<A, E>`.
  Named(name: Name, args: List(Type))
  /// Type variable. `index` is the alpha-renamed canonical position (used by
  /// matching, precomputed so no renaming is needed at query time); `name` is the
  /// source name kept for faithful display (for Gleam it is the conventional
  /// letter derived from the id — `a`, `b`, …; for TS/Java the real `<T, E>`).
  Var(index: Int, name: String)
  /// Query-side wildcard: matches any type.
  Hole
  Tuple(elements: List(Type))
  /// Anonymous structural shape: TS objects/interfaces, class field sets.
  Record(fields: List(Field))
  /// Anonymous union: `A | B`.
  Union(members: List(Type))
}

pub type Param {
  Param(label: Option(String), is_spread: Bool, type_: Type)
}

pub type Field {
  Field(name: String, type_: Type)
}

/// A possibly-qualified type name.
///   - `package` origin package (`""` for the prelude/builtins). Carried for
///               nominal linkability (query-time ref resolution); the matcher
///               ignores it by default. Not emitted in the canonical string.
///   - `module`  the module path (`""` for the prelude), e.g. `"gleam/option"`.
///   - `name`    the written name, e.g. `"Option"`.
///   - `concept` cross-language canonical id from the synonym table (layer 3),
///               e.g. `name "Int"` → `concept "int"`. Exact match uses `name`;
///               conceptual match uses `concept` when present.
pub type Name {
  Name(package: String, module: String, name: String, concept: Option(String))
}

/// A stored, searchable declaration. Only the *structural* core lives here; the
/// metadata (name, documentation, kind, deprecation, implementations) stays in
/// its own DB columns. Extend with a `Class` variant when OO languages land.
pub type Declaration {
  Function(signature: Type)
  Constant(type_: Type)
  TypeAlias(parameters: Int, aliased: Type)
  TypeDefinition(parameters: Int, constructors: List(Constructor))
}

pub type Constructor {
  Constructor(name: String, parameters: List(Param))
}

/// Deterministic printer: an IR `Type` → its canonical notation string.
/// Inverse of the canonical parser. See `signature-ir.md` for the grammar.
pub fn to_string(type_: Type) -> String {
  case type_ {
    Fn(params, return, is_method) -> print_fn(params, return, is_method)
    Named(name, args) -> print_named(name, args)
    Var(i, _) -> "$" <> int.to_string(i)
    Hole -> "_"
    Tuple(elements) -> "#(" <> print_args(elements) <> ")"
    Record(fields) -> "{ " <> print_fields(fields) <> " }"
    Union(members) ->
      members
      |> list.map(to_string)
      |> string.join(" | ")
  }
}

fn print_fn(params: List(Param), return: Type, is_method: Bool) -> String {
  let tail = ") -> " <> to_string(return)
  case is_method, params {
    True, [receiver, ..rest] ->
      to_string(receiver.type_) <> " :: (" <> print_params(rest) <> tail
    _, _ -> "(" <> print_params(params) <> tail
  }
}

fn print_params(params: List(Param)) -> String {
  params
  |> list.map(print_param)
  |> string.join(", ")
}

fn print_param(param: Param) -> String {
  let label = case param.label {
    Some(name) -> name <> ": "
    None -> ""
  }
  let spread = case param.is_spread {
    True -> "..."
    False -> ""
  }
  label <> spread <> to_string(param.type_)
}

fn print_named(named: Name, args: List(Type)) -> String {
  let qualified = case named.module {
    "" -> named.name
    module -> module <> "." <> named.name
  }
  case args {
    [] -> qualified
    args -> qualified <> "<" <> print_args(args) <> ">"
  }
}

fn print_args(args: List(Type)) -> String {
  args
  |> list.map(to_string)
  |> string.join(", ")
}

fn print_fields(fields: List(Field)) -> String {
  fields
  |> list.map(fn(field) { field.name <> ": " <> to_string(field.type_) })
  |> string.join(", ")
}

// Original variable id → canonical index, plus the next index to hand out.
type Rename =
  #(Dict(Int, Int), Int)

/// Alpha-rename type variables to first-appearance order (`0, 1, 2, …`), so that
/// alpha-equivalent signatures become identical (`fn(a, b) -> a` ≡ `fn(x, y) -> x`).
/// Front-ends emit `Var` with the source language's ids; this canonicalizes them.
/// Reading order: parameters left-to-right, then the return type.
pub fn alpha_rename(type_: Type) -> Type {
  let #(renamed, _) = rename(type_, #(dict.new(), 0))
  renamed
}

/// Alpha-rename a whole declaration. Variables are canonicalized across the
/// entire declaration in one numbering: a `TypeDefinition`'s constructors share
/// it (the variables are bound once at the type head), read left-to-right.
pub fn alpha_rename_declaration(declaration: Declaration) -> Declaration {
  case declaration {
    Function(signature:) -> Function(alpha_rename(signature))
    Constant(type_:) -> Constant(alpha_rename(type_))
    TypeAlias(parameters:, aliased:) ->
      TypeAlias(parameters:, aliased: alpha_rename(aliased))
    TypeDefinition(parameters:, constructors:) -> {
      let #(_, constructors) =
        list.map_fold(constructors, #(dict.new(), 0), fn(state, constructor) {
          let #(params, state) = rename_params(constructor.parameters, state)
          #(state, Constructor(..constructor, parameters: params))
        })
      TypeDefinition(parameters:, constructors:)
    }
  }
}

fn rename(type_: Type, state: Rename) -> #(Type, Rename) {
  case type_ {
    Var(index, name) -> {
      let #(mapping, next) = state
      case dict.get(mapping, index) {
        Ok(canonical) -> #(Var(canonical, name), state)
        Error(_) -> #(
          Var(next, name),
          #(dict.insert(mapping, index, next), next + 1),
        )
      }
    }
    Hole -> #(Hole, state)
    Named(name, args) -> {
      let #(args, state) = rename_types(args, state)
      #(Named(name, args), state)
    }
    Tuple(elements) -> {
      let #(elements, state) = rename_types(elements, state)
      #(Tuple(elements), state)
    }
    Union(members) -> {
      let #(members, state) = rename_types(members, state)
      #(Union(members), state)
    }
    Fn(params, return, is_method) -> {
      let #(params, state) = rename_params(params, state)
      let #(return, state) = rename(return, state)
      #(Fn(params, return, is_method), state)
    }
    Record(fields) -> {
      let #(fields, state) = rename_fields(fields, state)
      #(Record(fields), state)
    }
  }
}

fn rename_types(types: List(Type), state: Rename) -> #(List(Type), Rename) {
  let #(state, renamed) =
    list.map_fold(types, state, fn(state, type_) {
      let #(type_, state) = rename(type_, state)
      #(state, type_)
    })
  #(renamed, state)
}

fn rename_params(params: List(Param), state: Rename) -> #(List(Param), Rename) {
  let #(state, renamed) =
    list.map_fold(params, state, fn(state, param) {
      let #(type_, state) = rename(param.type_, state)
      #(state, Param(..param, type_:))
    })
  #(renamed, state)
}

fn rename_fields(fields: List(Field), state: Rename) -> #(List(Field), Rename) {
  let #(state, renamed) =
    list.map_fold(fields, state, fn(state, field) {
      let #(type_, state) = rename(field.type_, state)
      #(state, Field(..field, type_:))
    })
  #(renamed, state)
}

// ─── Fingerprint (phase-1 GIN candidate selection) ───────────────────────────
// Derived from the IR. `arity` is the top-level function's parameter count (0 for
// non-function forms), narrowing candidates by shape. `type_names` is the set of
// concrete nominal names the declaration mentions, nesting flattened; `Var` and
// `Hole` contribute nothing. A GIN index on `type_names` answers
// `arity = $1 AND type_names @> ARRAY[...]`. v1 stores the written `Name.name`;
// synonym/concept expansion is deferred to the matching tier.

pub fn fingerprint(declaration: Declaration) -> #(Int, List(String)) {
  case declaration {
    Function(signature:) -> fingerprint_type(signature)
    Constant(type_:) -> fingerprint_type(type_)
    TypeAlias(aliased:, ..) -> fingerprint_type(aliased)
    TypeDefinition(constructors:, ..) -> {
      let types =
        list.flat_map(constructors, fn(c) {
          list.map(c.parameters, fn(p) { p.type_ })
        })
      #(0, list.flat_map(types, collect_names(_, [])) |> list.unique)
    }
  }
}

/// Fingerprint a bare `Type` — the query side, where only a parsed type is in
/// hand and a declaration's metadata is absent. Uses the same arity and name
/// derivation the stored `fingerprint` applies to `Function`/`Constant`, so a
/// query and a candidate land on comparable fingerprints for the phase-1 filter.
pub fn fingerprint_type(type_: Type) -> #(Int, List(String)) {
  #(type_arity(type_), names(type_))
}

fn type_arity(type_: Type) -> Int {
  case type_ {
    Fn(params:, ..) -> list.length(params)
    _ -> 0
  }
}

fn names(type_: Type) -> List(String) {
  collect_names(type_, []) |> list.unique
}

fn collect_names(type_: Type, acc: List(String)) -> List(String) {
  case type_ {
    Named(name:, args:) ->
      list.fold(args, [name.name, ..acc], fn(acc, arg) {
        collect_names(arg, acc)
      })
    Fn(params:, return:, ..) ->
      params
      |> list.fold(acc, fn(acc, param) { collect_names(param.type_, acc) })
      |> collect_names(return, _)
    Tuple(elements:) ->
      list.fold(elements, acc, fn(acc, el) { collect_names(el, acc) })
    Union(members:) ->
      list.fold(members, acc, fn(acc, member) { collect_names(member, acc) })
    Record(fields:) ->
      list.fold(fields, acc, fn(acc, field) { collect_names(field.type_, acc) })
    Var(..) | Hole -> acc
  }
}

// ─── JSON codec (storage format of `signature_ir`) ───────────────────────────
// Structural only: metadata (name, documentation, kind, deprecation,
// implementations) lives in its own DB columns. Self-describing via a `kind`
// tag per node so `decoder()` round-trips `encode()` standalone.

pub fn encode(declaration: Declaration) -> Json {
  case declaration {
    Function(signature:) ->
      json.object([
        #("kind", json.string("function")),
        #("signature", encode_type(signature)),
      ])
    Constant(type_:) ->
      json.object([
        #("kind", json.string("constant")),
        #("type", encode_type(type_)),
      ])
    TypeAlias(parameters:, aliased:) ->
      json.object([
        #("kind", json.string("type-alias")),
        #("parameters", json.int(parameters)),
        #("aliased", encode_type(aliased)),
      ])
    TypeDefinition(parameters:, constructors:) ->
      json.object([
        #("kind", json.string("type-definition")),
        #("parameters", json.int(parameters)),
        #("constructors", json.array(constructors, encode_constructor)),
      ])
  }
}

fn encode_constructor(constructor: Constructor) -> Json {
  json.object([
    #("name", json.string(constructor.name)),
    #("parameters", json.array(constructor.parameters, encode_param)),
  ])
}

fn encode_type(type_: Type) -> Json {
  case type_ {
    Fn(params:, return:, is_method:) ->
      json.object([
        #("kind", json.string("fn")),
        #("params", json.array(params, encode_param)),
        #("return", encode_type(return)),
        #("is_method", json.bool(is_method)),
      ])
    Named(name:, args:) ->
      json.object([
        #("kind", json.string("named")),
        #("package", json.string(name.package)),
        #("module", json.string(name.module)),
        #("name", json.string(name.name)),
        #("concept", json.nullable(name.concept, json.string)),
        #("args", json.array(args, encode_type)),
      ])
    Var(index:, name:) ->
      json.object([
        #("kind", json.string("var")),
        #("index", json.int(index)),
        #("name", json.string(name)),
      ])
    Hole -> json.object([#("kind", json.string("hole"))])
    Tuple(elements:) ->
      json.object([
        #("kind", json.string("tuple")),
        #("elements", json.array(elements, encode_type)),
      ])
    Record(fields:) ->
      json.object([
        #("kind", json.string("record")),
        #("fields", json.array(fields, encode_field)),
      ])
    Union(members:) ->
      json.object([
        #("kind", json.string("union")),
        #("members", json.array(members, encode_type)),
      ])
  }
}

fn encode_param(param: Param) -> Json {
  json.object([
    #("label", json.nullable(param.label, json.string)),
    #("is_spread", json.bool(param.is_spread)),
    #("type", encode_type(param.type_)),
  ])
}

fn encode_field(field: Field) -> Json {
  json.object([
    #("name", json.string(field.name)),
    #("type", encode_type(field.type_)),
  ])
}

pub fn decoder() -> decode.Decoder(Declaration) {
  use kind <- decode.field("kind", decode.string)
  case kind {
    "function" -> {
      use signature <- decode.field("signature", type_decoder())
      decode.success(Function(signature:))
    }
    "constant" -> {
      use type_ <- decode.field("type", type_decoder())
      decode.success(Constant(type_:))
    }
    "type-alias" -> {
      use parameters <- decode.field("parameters", decode.int)
      use aliased <- decode.field("aliased", type_decoder())
      decode.success(TypeAlias(parameters:, aliased:))
    }
    "type-definition" -> {
      use parameters <- decode.field("parameters", decode.int)
      use constructors <- decode.field(
        "constructors",
        decode.list(constructor_decoder()),
      )
      decode.success(TypeDefinition(parameters:, constructors:))
    }
    _ -> decode.failure(Constant(Hole), "Declaration")
  }
}

fn constructor_decoder() -> decode.Decoder(Constructor) {
  use name <- decode.field("name", decode.string)
  use parameters <- decode.field("parameters", decode.list(param_decoder()))
  decode.success(Constructor(name:, parameters:))
}

fn type_decoder() -> decode.Decoder(Type) {
  use kind <- decode.field("kind", decode.string)
  case kind {
    "fn" -> {
      use params <- decode.field("params", decode.list(param_decoder()))
      use return <- decode.field("return", type_decoder())
      use is_method <- decode.field("is_method", decode.bool)
      decode.success(Fn(params:, return:, is_method:))
    }
    "named" -> {
      use package <- decode.field("package", decode.string)
      use module <- decode.field("module", decode.string)
      use name <- decode.field("name", decode.string)
      use concept <- decrypt.optional_field("concept", decode.string)
      use args <- decode.field("args", decode.list(type_decoder()))
      decode.success(Named(Name(package:, module:, name:, concept:), args))
    }
    "var" -> {
      use index <- decode.field("index", decode.int)
      use name <- decode.field("name", decode.string)
      decode.success(Var(index:, name:))
    }
    "hole" -> decode.success(Hole)
    "tuple" -> {
      use elements <- decode.field("elements", decode.list(type_decoder()))
      decode.success(Tuple(elements:))
    }
    "record" -> {
      use fields <- decode.field("fields", decode.list(field_decoder()))
      decode.success(Record(fields:))
    }
    "union" -> {
      use members <- decode.field("members", decode.list(type_decoder()))
      decode.success(Union(members:))
    }
    _ -> decode.failure(Hole, "Type")
  }
}

fn param_decoder() -> decode.Decoder(Param) {
  use label <- decrypt.optional_field("label", decode.string)
  use is_spread <- decode.field("is_spread", decode.bool)
  use type_ <- decode.field("type", type_decoder())
  decode.success(Param(label:, is_spread:, type_:))
}

fn field_decoder() -> decode.Decoder(Field) {
  use name <- decode.field("name", decode.string)
  use type_ <- decode.field("type", type_decoder())
  decode.success(Field(name:, type_:))
}
