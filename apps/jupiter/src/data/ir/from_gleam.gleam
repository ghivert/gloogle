//// Gleam front-end: converts a `package_interface` type into the language-agnostic
//// signature IR (`data/ir`). One `ir/from_<language>` converter per source language.
//// Pure — no `ctx`, no DB (unlike `generate/types`, which also resolves cross-package
//// refs). Mirrors the recursion of `generate/sources`/`generate/types`.
////
//// Type variables keep their `pi` ids here (`Var(pi_id)`); the shared
//// alpha-renaming pass normalizes them to first-appearance order afterwards.

import data/ir
import gleam/list
import gleam/option.{None}
import gleam/package_interface as pi
import gleam/string

/// Lower a function signature into an `ir.Fn`. Top-level parameters carry labels.
pub fn function(function: pi.Function) -> ir.Type {
  ir.Fn(
    params: list.map(function.parameters, parameter),
    return: type_(function.return),
    is_method: False,
  )
}

/// Lower a constant's type.
pub fn constant(constant: pi.Constant) -> ir.Type {
  type_(constant.type_)
}

fn parameter(parameter: pi.Parameter) -> ir.Param {
  ir.Param(
    label: parameter.label,
    is_spread: False,
    type_: type_(parameter.type_),
  )
}

/// The recursive workhorse: `pi.Type` → `ir.Type`.
pub fn type_(t: pi.Type) -> ir.Type {
  case t {
    pi.Tuple(elements) -> ir.Tuple(list.map(elements, type_))
    // A function *type* (higher-order); its parameters carry no labels.
    pi.Fn(parameters, return) ->
      ir.Fn(
        params: list.map(parameters, fn(t) { ir.Param(None, False, type_(t)) }),
        return: type_(return),
        is_method: False,
      )
    pi.Variable(id) -> ir.Var(index: id, name: variable_name(id))
    pi.Named(name, package, module, parameters) ->
      ir.Named(
        name: named(name, package, module),
        args: list.map(parameters, type_),
      )
  }
}

fn named(name: String, package: String, module: String) -> ir.Name {
  let module = module_path(package, module)
  ir.Name(package:, module:, name:, concept: None)
}

/// Prelude types (`package == ""`, `module == "gleam"`) are unqualified; other
/// types keep their module path.
fn module_path(package: String, module: String) -> String {
  case is_prelude(package, module) {
    True -> ""
    False -> module
  }
}

fn is_prelude(package: String, module: String) -> Bool {
  package == "" && module == "gleam"
}

/// Gleam type variables have no source name; the compiler assigns ids and the
/// display name is the conventional letter (`0 → a`, `1 → b`, …), mirroring
/// `generate/sources`.
fn variable_name(id: Int) -> String {
  case string.utf_codepoint(97 + id) {
    Ok(codepoint) -> string.from_utf_codepoints([codepoint])
    Error(_) -> "a"
  }
}
