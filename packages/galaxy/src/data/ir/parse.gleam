//// Query-side parser — Gleam type syntax → the searchable `ir.Type`.
////
//// This is the *query* front-end for Gleam: it turns what a user types into the
//// search box (`fn(Result(a, b), fn(a) -> c) -> Result(c, b)`, `List(a)`,
//// `#(Int, String)`) into the same IR that the ingestion front-end
//// (`data/ir/from_gleam`, in jupiter) derives from the structured package
//// interface. Both sides must land on the same canonical form, or a query could
//// never unify against a stored signature.
////
//// It lives in `galaxy`, not jupiter, on purpose: the frontend (venus) reuses it
//// to parse queries client-side. It only needs a text lexer (`glexer`) and a
//// parser-combinator library (`chomp`) — never `gleam_package_interface`, the one
//// dependency kept out of galaxy — so it crosses to the JavaScript target with
//// the rest of the package.
////
//// Conventions the Gleam surface encodes, mirrored here:
////   - a lower-case name is a type variable (`a`, `b`), alpha-renamed to a
////     first-appearance index exactly as the stored IR is;
////   - `_` is the query wildcard → `ir.Hole` (each occurrence independent);
////   - `Upper` / `module.Upper` is a nominal type; `Name(args)` applies it;
////   - `#(…)` is a tuple, `fn(…) -> …` a function.
//// Gleam's surface has no anonymous records or unions, so this parser never
//// produces `ir.Record`/`ir.Union` — those arrive with the TS front-end.

import chomp.{do, return}
import chomp/lexer
import chomp/span
import data/ir
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import glexer
import glexer/token

/// Parse a user-typed Gleam type into the canonical, alpha-renamed `ir.Type`.
/// `Error(Nil)` on a syntax the grammar does not accept.
pub fn parse(input: String) -> Result(ir.Type, Nil) {
  let tokens =
    input
    |> glexer.new
    |> glexer.discard_whitespace
    |> glexer.discard_comments
    |> glexer.lex
    |> list.map(fn(elem) {
      lexer.Token(span.Span({ elem.1 }.byte_offset, 0, 0, 0), "", elem.0)
    })
  parse_query()
  |> chomp.run(tokens, _)
  |> result.replace_error(Nil)
  |> result.map(canonicalize)
}

/// A whole query is a single type and nothing after it: `chomp.run` stops at the
/// first success and would otherwise ignore trailing tokens, so `List(a) junk`
/// must be rejected, not silently read as `List(a)`.
fn parse_query() {
  use type_ <- do(parse_type())
  use _ <- do(chomp.end())
  return(type_)
}

// ─── Grammar ─────────────────────────────────────────────────────────────────

fn parse_type() {
  chomp.one_of([
    parse_fn(),
    parse_tuple(),
    chomp.backtrackable(parse_named()),
    parse_var(),
  ])
}

/// `fn(params) -> return`. Query functions are never methods — the receiver-first
/// convergence is a lowering concern, done when the front-ends emit it, not here.
fn parse_fn() {
  use _ <- do(chomp.token(token.Fn))
  use _ <- do(chomp.token(token.LeftParen))
  use params <- do(chomp.sequence(parse_param(), separator: comma()))
  use _ <- do(chomp.token(token.RightParen))
  use _ <- do(chomp.token(token.RightArrow))
  use return_ <- do(parse_type())
  return(ir.Fn(params:, return: return_, is_method: False))
}

fn parse_param() {
  use label <- do(chomp.optional(parse_label()))
  use type_ <- do(parse_type())
  return(ir.Param(label:, is_spread: False, type_:))
}

/// A leading `label:` on a parameter. Backtrackable: a bare `a` is a variable,
/// not a label, and only the trailing `:` tells them apart.
fn parse_label() {
  chomp.backtrackable({
    use name <- do(parse_lower_name())
    use _ <- do(chomp.token(token.Colon))
    return(name)
  })
}

/// `#(A, B, …)`.
fn parse_tuple() {
  use _ <- do(chomp.token(token.Hash))
  use _ <- do(chomp.token(token.LeftParen))
  use elements <- do(chomp.sequence(parse_type(), separator: comma()))
  use _ <- do(chomp.token(token.RightParen))
  return(ir.Tuple(elements))
}

/// A nominal type, possibly qualified and applied: `Int`, `List(a)`,
/// `option.Option(a)`, `gleam/option.Option(a)`.
fn parse_named() {
  use #(module, name) <- do(parse_upper_name())
  use args <- do(chomp.optional(parse_type_args()))
  chomp.succeed(ir.Named(
    ir.Name(package: "", module:, name:, concept: None),
    option.unwrap(args, []),
  ))
}

/// `(A, B, …)` — Gleam applies type arguments with parentheses, not `<…>`.
fn parse_type_args() {
  use _ <- do(chomp.token(token.LeftParen))
  use args <- do(chomp.sequence(parse_type(), separator: comma()))
  use _ <- do(chomp.token(token.RightParen))
  return(args)
}

/// The `[segment/]*[module.]Upper` head of a nominal type. Returns the module
/// path (`""` when unqualified) and the written name. The matcher compares names
/// alone, but the module is kept for nominal linkability.
fn parse_upper_name() {
  use segments <- do(
    chomp.optional(chomp.sequence(parse_lower_name(), separator: slash())),
  )
  use _ <- do(chomp.optional(chomp.token(token.Dot)))
  use name <- chomp.take_map()
  case name {
    token.UpperName(content) -> {
      let module = string.join(option.unwrap(segments, []), "/")
      Some(#(module, content))
    }
    _ -> None
  }
}

/// A lower-case name → a type variable (index assigned later, in `canonicalize`);
/// `_` → the query wildcard.
fn parse_var() {
  use tok <- chomp.take_map()
  case tok {
    token.Name(content) -> Some(ir.Var(0, content))
    token.DiscardName(_) -> Some(ir.Hole)
    _ -> None
  }
}

fn parse_lower_name() {
  use tok <- chomp.take_map()
  case tok {
    token.Name(content) -> Some(content)
    _ -> None
  }
}

fn comma() {
  chomp.token(token.Comma)
}

fn slash() {
  chomp.token(token.Slash)
}

// ─── Alpha-renaming ──────────────────────────────────────────────────────────
// The parser tags every variable with its source name and a placeholder index;
// this pass replaces the name with a first-appearance index (params left-to-
// right, then the return), the same canonical numbering the stored IR carries,
// so a query and a candidate line up on `Var.index`. Distinct names get distinct
// indices; repeated names share one; each `_` stays an independent `Hole`.

type Vars =
  #(Dict(String, Int), Int)

fn canonicalize(type_: ir.Type) -> ir.Type {
  let #(renamed, _) = canon(type_, #(dict.new(), 0))
  renamed
}

fn canon(type_: ir.Type, vars: Vars) -> #(ir.Type, Vars) {
  case type_ {
    ir.Var(_, name) -> {
      let #(mapping, next) = vars
      case dict.get(mapping, name) {
        Ok(index) -> #(ir.Var(index, name), vars)
        Error(_) -> #(
          ir.Var(next, name),
          #(dict.insert(mapping, name, next), next + 1),
        )
      }
    }
    ir.Hole -> #(ir.Hole, vars)
    ir.Named(name, args) -> {
      let #(args, vars) = canon_list(args, vars)
      #(ir.Named(name, args), vars)
    }
    ir.Tuple(elements) -> {
      let #(elements, vars) = canon_list(elements, vars)
      #(ir.Tuple(elements), vars)
    }
    ir.Fn(params, return_, is_method) -> {
      let #(params, vars) = canon_params(params, vars)
      let #(return_, vars) = canon(return_, vars)
      #(ir.Fn(params, return_, is_method), vars)
    }
    ir.Union(members) -> {
      let #(members, vars) = canon_list(members, vars)
      #(ir.Union(members), vars)
    }
    ir.Record(fields) -> {
      let #(fields, vars) = canon_fields(fields, vars)
      #(ir.Record(fields), vars)
    }
  }
}

fn canon_list(types: List(ir.Type), vars: Vars) -> #(List(ir.Type), Vars) {
  let #(vars, renamed) =
    list.map_fold(types, vars, fn(vars, type_) {
      let #(type_, vars) = canon(type_, vars)
      #(vars, type_)
    })
  #(renamed, vars)
}

fn canon_params(params: List(ir.Param), vars: Vars) -> #(List(ir.Param), Vars) {
  let #(vars, renamed) =
    list.map_fold(params, vars, fn(vars, param) {
      let #(type_, vars) = canon(param.type_, vars)
      #(vars, ir.Param(..param, type_:))
    })
  #(renamed, vars)
}

fn canon_fields(fields: List(ir.Field), vars: Vars) -> #(List(ir.Field), Vars) {
  let #(vars, renamed) =
    list.map_fold(fields, vars, fn(vars, field) {
      let #(type_, vars) = canon(field.type_, vars)
      #(vars, ir.Field(..field, type_:))
    })
  #(renamed, vars)
}
