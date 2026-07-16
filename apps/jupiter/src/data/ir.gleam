//// Signature IR — the shared, language-agnostic representation of a type that
//// `type_search` matches on. See `signature-ir.md` for the full design.
////
//// A searchable signature is a `Type` (usually an `Fn`). Language front-ends
//// lower their surface syntax into this IR; the canonical notation parser/printer
//// round-trips it. Two invariants:
////   - type variables are alpha-renamed to indices (`Var(Int)`), the source name
////     is dropped;
////   - a method is a receiver-first `Fn` with `method: True`.

import gleam/dict.{type Dict}
import gleam/int
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
