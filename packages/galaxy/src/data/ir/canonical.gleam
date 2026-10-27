//// The canonical-notation parser — the inverse of `ir.to_string`.
////
//// `ir.to_string` prints a type in the notation `signature-ir.md` fixes in BNF:
//// variables are `$n`, the wildcard is `_`, a nominal type applies its
//// arguments with `<…>` (`Result<$0, $1>`), a function is `(params) -> return`,
//// a method `recv :: (params) -> return`, a tuple `#(…)`, a record `{ a: T }`,
//// a union `A | B`. This module reads that notation back into the IR, so that
//// `parse(to_string(t))` is `t` (up to the source names of variables, which the
//// notation does not carry — a parsed `Var` has an empty `name`).
////
//// Two uses. The round-trip law in the property tests, which is what proves the
//// printer loses nothing. And a language-neutral query syntax: the Gleam query
//// parser (`data/ir/parse`) reads what a Gleam user types, this one reads the
//// stored normal form (`signature_canonical`), the same whatever the source
//// language.
////
//// The notation is read exactly as printed: a function nested in a union, a
//// union returned by a function, and a receiver that is itself a function or
//// a union are parenthesised by the printer, and `( type )` is a grouping here.
//// Bare, `(A) -> B | C` is a function returning `B | C` (a return extends as
//// far as it can); the printer never writes it that way.
//// Unlike the Gleam parser, variables are *not* alpha-renamed on the way in —
//// the indices are what the string carries.

import chomp.{do, return}
import chomp/lexer
import data/ir
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/set
import gleam/string

pub type Token {
  LeftParen
  RightParen
  HashParen
  LeftBrace
  RightBrace
  Less
  Greater
  Comma
  Pipe
  Colon
  DoubleColon
  Dot
  Slash
  Arrow
  Spread
  Underscore
  /// `$n`.
  Variable(Int)
  /// A `$` not followed by digits.
  BadVariable
  Upper(String)
  Lower(String)
}

/// Parse a type written in the canonical notation. `Error(Nil)` on anything the
/// grammar does not accept, including a duplicate record field name.
pub fn parse(input: String) -> Result(ir.Type, Nil) {
  use tokens <- result.try(
    lexer.run(input, lexer()) |> result.replace_error(Nil),
  )
  parse_whole()
  |> chomp.run(tokens, _)
  |> result.replace_error(Nil)
}

// ─── Lexer ───────────────────────────────────────────────────────────────────

fn lexer() {
  lexer.simple([
    lexer.whitespace(Nil) |> lexer.ignore,
    lexer.token("#(", HashParen),
    lexer.token("(", LeftParen),
    lexer.token(")", RightParen),
    lexer.token("{", LeftBrace),
    lexer.token("}", RightBrace),
    lexer.token("<", Less),
    lexer.token(">", Greater),
    lexer.token(",", Comma),
    lexer.token("|", Pipe),
    lexer.token("->", Arrow),
    lexer.token("::", DoubleColon),
    lexer.symbol(":", "[^:]", Colon),
    lexer.token("...", Spread),
    lexer.symbol(".", "[^.]", Dot),
    lexer.token("/", Slash),
    lexer.symbol("_", "[^a-zA-Z0-9_]", Underscore),
    lexer.identifier("\\$", "[0-9]", set.new(), fn(lexeme) {
      lexeme
      |> string.drop_start(1)
      |> int.parse
      |> result.map(Variable)
      |> result.unwrap(BadVariable)
    }),
    lexer.identifier("[A-Z]", "[a-zA-Z0-9_]", set.new(), Upper),
    lexer.identifier("[a-z]", "[a-zA-Z0-9_]", set.new(), Lower),
  ])
}

// ─── Grammar ─────────────────────────────────────────────────────────────────

/// A whole input is one type and nothing after it.
fn parse_whole() {
  use type_ <- do(parse_type())
  use _ <- do(chomp.end())
  return(type_)
}

/// `type ::= atom { "|" atom }` — a single member is the member itself.
/// Duplicate members collapse: `Int | Int` is `Int`, as the unifier, which
/// matches members as a set, already assumes.
fn parse_type() {
  use first <- do(chomp.lazy(parse_atom))
  use rest <- do(
    chomp.many({
      use _ <- do(chomp.token(Pipe))
      chomp.lazy(parse_atom)
    }),
  )
  case list.unique([first, ..rest]) {
    [single] -> return(single)
    members -> return(ir.Union(members))
  }
}

fn parse_atom() {
  chomp.one_of([
    chomp.backtrackable(parse_method()),
    chomp.backtrackable(parse_fn()),
    parse_tuple(),
    parse_record(),
    parse_named(),
    parse_var(),
    parse_hole(),
    parse_group(),
  ])
}

/// `recv "::" "(" params ")" "->" type`. The receiver is a parameter like the
/// others: bare, it is an atom with no label and no spread; with either, or
/// when it is a function or a union, it comes parenthesised with them —
/// `(self: ...Int) :: (…) -> …` — as the printer writes it. A bare function
/// cannot start the method production: that would make the grammar
/// left-recursive (a method is an atom whose first thing is an atom).
fn parse_method() {
  use receiver <- do(parse_receiver())
  use _ <- do(chomp.token(DoubleColon))
  use function <- do(parse_fn())
  case function {
    ir.Fn(params:, return: return_, ..) ->
      return(ir.Fn(
        params: [receiver, ..params],
        return: return_,
        is_method: True,
      ))
    _ -> chomp.fail(Nil)
  }
}

fn parse_receiver() {
  let bare =
    chomp.one_of([
      parse_tuple(),
      parse_record(),
      parse_named(),
      parse_var(),
      parse_hole(),
    ])
    |> chomp.map(fn(type_) { ir.Param(None, False, type_) })
  chomp.one_of([chomp.backtrackable(parse_parenthesised_receiver()), bare])
}

/// `"(" [label ":"] ["..."] type ")"` — a plain group reads the same way, with
/// neither label nor spread.
fn parse_parenthesised_receiver() {
  use _ <- do(chomp.token(LeftParen))
  use label <- do(chomp.optional(parse_label()))
  use spread <- do(chomp.optional(chomp.token(Spread)))
  use type_ <- do(chomp.lazy(parse_type))
  use _ <- do(chomp.token(RightParen))
  return(ir.Param(label:, is_spread: option.is_some(spread), type_:))
}

/// `"(" params ")" "->" type`.
fn parse_fn() {
  use _ <- do(chomp.token(LeftParen))
  use params <- do(chomp.sequence(parse_param(), separator: comma()))
  use _ <- do(chomp.token(RightParen))
  use _ <- do(chomp.token(Arrow))
  use return_ <- do(chomp.lazy(parse_type))
  return(ir.Fn(params:, return: return_, is_method: False))
}

/// `[label ":"] ["..."] type`.
fn parse_param() {
  use label <- do(chomp.optional(parse_label()))
  use spread <- do(chomp.optional(chomp.token(Spread)))
  use type_ <- do(chomp.lazy(parse_type))
  return(ir.Param(label:, is_spread: option.is_some(spread), type_:))
}

/// Backtrackable: a bare `a` would be a field name only in a record, but a label
/// and a lower-case module segment both start with a lower-case name, and only
/// the `:` tells the label apart.
fn parse_label() {
  chomp.backtrackable({
    use name <- do(parse_lower())
    use _ <- do(chomp.token(Colon))
    return(name)
  })
}

/// `#(A, B, …)`.
fn parse_tuple() {
  use _ <- do(chomp.token(HashParen))
  use elements <- do(chomp.sequence(chomp.lazy(parse_type), separator: comma()))
  use _ <- do(chomp.token(RightParen))
  return(ir.Tuple(elements))
}

/// `{ name: Type, … }` — field names unique, as the unifier (which matches them
/// by name) assumes.
fn parse_record() {
  use _ <- do(chomp.token(LeftBrace))
  use fields <- do(chomp.sequence(parse_field(), separator: comma()))
  use _ <- do(chomp.token(RightBrace))
  let names = list.map(fields, fn(field) { field.name })
  case list.length(list.unique(names)) == list.length(names) {
    True -> return(ir.Record(fields))
    False -> chomp.fail(Nil)
  }
}

fn parse_field() {
  use name <- do(parse_field_name())
  use _ <- do(chomp.token(Colon))
  use type_ <- do(chomp.lazy(parse_type))
  return(ir.Field(name:, type_:))
}

fn parse_field_name() {
  use tok <- chomp.take_map()
  case tok {
    Lower(name) | Upper(name) -> Some(name)
    _ -> None
  }
}

/// `[segment "/" …] [module "."] Upper [ "<" args ">" ]`. The module path is kept
/// (`gleam/option.Option` → module `gleam/option`), the package is unknown.
fn parse_named() {
  use module <- do(chomp.optional(chomp.backtrackable(parse_module())))
  use name <- do(parse_upper())
  use args <- do(chomp.optional(parse_args()))
  return(ir.Named(
    ir.Name(
      package: "",
      module: option.unwrap(module, ""),
      name:,
      concept: None,
    ),
    option.unwrap(args, []),
  ))
}

fn parse_module() {
  use segments <- do(chomp.sequence(parse_lower(), separator: slash()))
  use _ <- do(chomp.token(Dot))
  case segments {
    [] -> chomp.fail(Nil)
    _ -> return(string.join(segments, "/"))
  }
}

fn parse_args() {
  use _ <- do(chomp.token(Less))
  use args <- do(chomp.sequence(chomp.lazy(parse_type), separator: comma()))
  use _ <- do(chomp.token(Greater))
  return(args)
}

/// `$n` → a variable with that index and no source name.
fn parse_var() {
  use tok <- chomp.take_map()
  case tok {
    Variable(index) -> Some(ir.Var(index, ""))
    _ -> None
  }
}

fn parse_hole() {
  use _ <- do(chomp.token(Underscore))
  return(ir.Hole)
}

/// `( type )` — grouping, written by the printer wherever the notation would
/// otherwise be ambiguous.
fn parse_group() {
  use _ <- do(chomp.token(LeftParen))
  use type_ <- do(chomp.lazy(parse_type))
  use _ <- do(chomp.token(RightParen))
  return(type_)
}

fn parse_upper() {
  use tok <- chomp.take_map()
  case tok {
    Upper(name) -> Some(name)
    _ -> None
  }
}

fn parse_lower() {
  use tok <- chomp.take_map()
  case tok {
    Lower(name) -> Some(name)
    _ -> None
  }
}

fn comma() {
  chomp.token(Comma)
}

fn slash() {
  chomp.token(Slash)
}
