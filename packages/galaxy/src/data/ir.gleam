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
import gleam/set
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

/// Deterministic printer: an IR `Type` → its canonical notation string, as
/// written — module qualification, labels, the `recv :: (…)` method form.
/// Inverse of the canonical parser (`data/ir/canonical`): `parse(to_string(t))`
/// is `t` up to variable names. See `signature-ir.md` for the grammar. Where the
/// notation would be ambiguous the printer parenthesises: a function nested in
/// a union (`((A) -> B) | C`), a union returned by a function
/// (`(A) -> (B | C)`), a receiver that is a function or a union.
pub fn to_string(type_: Type) -> String {
  print(type_, Written)
}

/// The *normal form* of a type: one string per class of types the exact tier
/// (`unify`'s `Equivalent`) treats as equal, so that alpha-equivalence of two
/// hole-free, label-free types is string equality. The stored
/// `signature_canonical` column answers the exact bucket with a btree lookup,
/// whatever the names' frequency, and never loses an exact match to phase 1's
/// `LIMIT 100`. Four normalisations on top of `to_string`, each one a liberty
/// the unifier takes that the written form does not:
///
///   - `Union` members and `Record` fields are sorted (the unifier matches them
///     as sets, the written form keeps list order); a nested union is flattened
///     and duplicate members dropped on the way (`A | (A | B)` is `A | B`);
///   - `is_method` is dropped (a receiver-first method and its function form
///     are one type to the unifier);
///   - labels are dropped (a label the query omits matches any candidate label);
///   - the `module.` qualification is dropped (the unifier compares bare names).
///
/// Then alpha-renamed in the *normalised* reading order and printed. `Union`
/// members are sorted by a variable-blind key, so the order does not depend on
/// the variable numbering it in turn decides; members that differ only in their
/// variables can still get two spellings for one class of types — a known
/// limit, irrelevant to Gleam, whose surface produces no `Union`.
pub fn to_canonical(type_: Type) -> String {
  type_
  |> normalise
  |> alpha_rename
  |> print(Canonical)
}

/// The normal form of a stored declaration — what `unify_declaration` matches
/// the query against. `None` for a `TypeDefinition`, which never verifies by
/// shape (its constructors are found by name).
pub fn canonical(declaration: Declaration) -> Option(String) {
  case declaration {
    Function(signature:) -> Some(to_canonical(signature))
    Constant(type_:) -> Some(to_canonical(type_))
    TypeAlias(aliased:, ..) -> Some(to_canonical(aliased))
    TypeDefinition(..) -> None
  }
}

/// `to_canonical` before renaming: sets sorted, labels and `is_method` gone.
fn normalise(type_: Type) -> Type {
  case type_ {
    Fn(params, return, _) -> {
      let params =
        list.map(params, fn(param) {
          Param(
            label: None,
            is_spread: param.is_spread,
            type_: normalise(param.type_),
          )
        })
      Fn(params, normalise(return), False)
    }
    Named(name, args) -> Named(name, list.map(args, normalise))
    Var(..) | Hole -> type_
    Tuple(elements) -> Tuple(list.map(elements, normalise))
    Record(fields) ->
      fields
      |> list.map(fn(field) { Field(field.name, normalise(field.type_)) })
      |> list.sort(fn(a, b) { string.compare(a.name, b.name) })
      |> Record
    Union(members) ->
      case
        members
        |> list.flat_map(flatten_union)
        |> list.map(normalise)
        // Duplicates as the normal form spells them: `gleam.List` and `List`
        // are one member once the qualification is gone.
        |> unique_by(print(_, Canonical))
        |> list.sort(fn(a, b) {
          string.compare(print(a, Blind), print(b, Blind))
        })
      {
        [single] -> single
        members -> Union(members)
      }
  }
}

/// A union's members are not unions: `A | (B | C)` is `A | B | C`, and the
/// printer writes nested unions flat anyway.
fn flatten_union(type_: Type) -> List(Type) {
  case type_ {
    Union(members) -> list.flat_map(members, flatten_union)
    other -> [other]
  }
}

/// The first item of each group sharing a key, in order.
fn unique_by(items: List(a), key: fn(a) -> String) -> List(a) {
  let #(kept, _) =
    list.fold(items, #([], set.new()), fn(acc, item) {
      let #(kept, seen) = acc
      let k = key(item)
      case set.contains(seen, k) {
        True -> acc
        False -> #([item, ..kept], set.insert(seen, k))
      }
    })
  list.reverse(kept)
}

/// How `print` spells a type.
type Spelling {
  /// As written: `module.Name`, labels, `recv :: (…) -> r` for a method.
  Written
  /// The normal form: bare names, no labels, function form only.
  Canonical
  /// `Canonical` with every variable printed as `$` — the sort key that keeps
  /// `Union` member order independent of the variable numbering.
  Blind
}

fn print(type_: Type, spelling: Spelling) -> String {
  case type_ {
    Fn(params, return, is_method) ->
      print_fn(params, return, is_method, spelling)
    Named(name, args) -> print_named(name, args, spelling)
    Var(i, _) ->
      case spelling {
        Blind -> "$"
        _ -> "$" <> int.to_string(i)
      }
    Hole -> "_"
    Tuple(elements) -> "#(" <> print_args(elements, spelling) <> ")"
    Record(fields) -> "{ " <> print_fields(fields, spelling) <> " }"
    Union(members) ->
      members
      |> list.map(print_member(_, spelling))
      |> string.join(" | ")
  }
}

/// A union member: a function is parenthesised, or `(A) -> B | C` would read as
/// a function returning `B | C`.
fn print_member(type_: Type, spelling: Spelling) -> String {
  case type_ {
    Fn(..) -> "(" <> print(type_, spelling) <> ")"
    _ -> print(type_, spelling)
  }
}

fn print_fn(
  params: List(Param),
  return: Type,
  is_method: Bool,
  spelling: Spelling,
) -> String {
  let tail = ") -> " <> print_return(return, spelling)
  case spelling, is_method, params {
    Written, True, [receiver, ..rest] ->
      print_receiver(receiver, spelling)
      <> " :: ("
      <> print_params(rest, spelling)
      <> tail
    _, _, _ -> "(" <> print_params(params, spelling) <> tail
  }
}

/// The receiver is a parameter like any other — its label and spread marker
/// are printed too, or the method form would lose them — and one that carries
/// either is parenthesised with them, `(self: ...Int) :: (…) -> …`: written
/// bare as a parameter's type, `self: ...Int :: (…)` would hand its label and
/// spread to the enclosing parameter. A composite type (function, union) is
/// parenthesised for the same reason as a return.
fn print_receiver(receiver: Param, spelling: Spelling) -> String {
  let label = case receiver.label {
    Some(name) -> name <> ": "
    None -> ""
  }
  let spread = case receiver.is_spread {
    True -> "..."
    False -> ""
  }
  case label <> spread, receiver.type_ {
    "", Union(..) | "", Fn(..) -> "(" <> print(receiver.type_, spelling) <> ")"
    "", _ -> print(receiver.type_, spelling)
    prefix, _ -> "(" <> prefix <> print(receiver.type_, spelling) <> ")"
  }
}

fn print_return(type_: Type, spelling: Spelling) -> String {
  case type_ {
    Union(..) -> "(" <> print(type_, spelling) <> ")"
    _ -> print(type_, spelling)
  }
}

fn print_params(params: List(Param), spelling: Spelling) -> String {
  params
  |> list.map(print_param(_, spelling))
  |> string.join(", ")
}

fn print_param(param: Param, spelling: Spelling) -> String {
  let label = case spelling, param.label {
    Written, Some(name) -> name <> ": "
    _, _ -> ""
  }
  let spread = case param.is_spread {
    True -> "..."
    False -> ""
  }
  label <> spread <> print(param.type_, spelling)
}

fn print_named(named: Name, args: List(Type), spelling: Spelling) -> String {
  let qualified = case spelling, named.module {
    Written, module if module != "" -> module <> "." <> named.name
    _, _ -> named.name
  }
  case args {
    [] -> qualified
    args -> qualified <> "<" <> print_args(args, spelling) <> ">"
  }
}

fn print_args(args: List(Type), spelling: Spelling) -> String {
  args
  |> list.map(print(_, spelling))
  |> string.join(", ")
}

fn print_fields(fields: List(Field), spelling: Spelling) -> String {
  fields
  |> list.map(fn(field) { field.name <> ": " <> print(field.type_, spelling) })
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

/// The nominal names a type mentions, deduplicated and **sorted**: the stored
/// `type_names` is a canonical set, so two fingerprints with the same names are
/// equal arrays — what the phase-1 subset lookup (btree equality on
/// `(arity, type_names)`) relies on.
fn names(type_: Type) -> List(String) {
  collect_names(type_, [])
  |> list.unique
  |> list.sort(string.compare)
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
