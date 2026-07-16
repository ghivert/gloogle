//// Signature IR — the shared, language-agnostic representation of a type that
//// `type_search` matches on. See `signature-ir.md` for the full design.
////
//// A searchable signature is a `Type` (usually an `Fn`). Language front-ends
//// lower their surface syntax into this IR; the canonical notation parser/printer
//// round-trips it. Two invariants:
////   - type variables are alpha-renamed to indices (`Var(Int)`), the source name
////     is dropped;
////   - a method is a receiver-first `Fn` with `method: True`.

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Type {
  /// `is_method: True` ⇒ the first parameter is the receiver.
  Fn(params: List(Param), return: Type, is_method: Bool)
  /// Nominal type applied to zero or more arguments: `Int`, `List<a>`, `Result<A, E>`.
  Named(name: Name, args: List(Type))
  /// Alpha-renamed type variable. No source name is kept.
  Var(index: Int)
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
///   - `qualifier` namespace segments, e.g. `["gleam", "option"]`.
///   - `base`      the written name, e.g. `"Option"`.
///   - `concept`   cross-language canonical id from the synonym table (layer 3),
///                 e.g. `base "Int"` → `concept "int"`. Exact match uses `base`;
///                 conceptual match uses `concept` when present.
pub type Name {
  Name(qualifier: List(String), base: String, concept: Option(String))
}

/// Deterministic printer: an IR `Type` → its canonical notation string.
/// Inverse of the canonical parser. See `signature-ir.md` for the grammar.
pub fn to_string(type_: Type) -> String {
  case type_ {
    Fn(params, return, is_method) -> print_fn(params, return, is_method)
    Named(name, args) -> print_named(name, args)
    Var(i) -> "$" <> int.to_string(i)
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

fn print_named(name: Name, args: List(Type)) -> String {
  let qualified = case name.qualifier {
    [] -> name.base
    qualifier -> string.join(list.append(qualifier, [name.base]), ".")
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
