//// Chomp takes a different approach to many other parser combinator libraries
//// by also providing a _lexer_ combinator module that you use to turn an input
//// string into a list of tokens.
////
//// Parser combinators are a powerful and flexible way to build parsers, but
//// they offer come at a performance cost compared to hand-written parsers or
//// parser generators. On the other hand, writing a lexer by hand can be a bit
//// tedious and difficult.
////
//// Chomp aims to provide a happy middle-ground by making it easy to produce
//// OK-performing lexers and then use parser combinators that can be much faster
//// working on the smaller token stream.
////

// IMPORTS ---------------------------------------------------------------------

import chomp/span.{type Span, Span}
import gleam/float
import gleam/int
import gleam/list
import gleam/regexp
import gleam/result
import gleam/set.{type Set}
import gleam/string

// TYPES -----------------------------------------------------------------------

/// A `Matcher` is how we define the rules that match parts of the input string
/// and turn them into tokens. At it's core, a `Match` is a function that takes
/// three arguments:
///
/// - The current mode of the lexer
///
/// - Any input we've accumulated so far
///
/// - A lookahead of one grapheme
///
/// With just these three arguments we can define arbitrary rules for consuming
/// (or not) input and producing tokens!
///
pub opaque type Matcher(a, mode) {
  Matcher(run: fn(mode, String, String) -> Match(a, mode))
}

/// When writing a custom matcher, a `Match` is what you return to tell the lexer
/// what to do next.
///
pub type Match(a, mode) {
  /// Consume the accumulated input and produce a token with the given value. A
  /// `Keep` match can also transition the lexer into a new mode.
  Keep(a, mode)
  /// Skip running any additional matchers this iteration, add the next grapheme
  /// to the accumulated input, and run the next iteration. A `Skip` match can
  /// also transition the lexer into a new mode.
  Skip(mode)
  /// Drop the accumulated input and move on to the next iteration. A `Drop`
  /// match can also transition the lexer into a new mode. This match is useful
  /// for discarding input like whitespace or comments.
  Drop(mode)
  /// The matcher did not match the input, so the lexer should try the next
  /// matcher in the list (or fail if there are no more matchers).
  NoMatch
}

/// You use chomp's lexer to turn a string into a list of tokens that your parser
/// will eventually consume. The `Token` type contains the lexeme that was consumed
/// (aka the raw input string), the source [`Span`](./span.html#Span) of the
/// consumed lexeme to locate it in the source, and whatever token value your lexer
/// produces.
///
pub type Token(a) {
  Token(span: Span, lexeme: String, value: a)
}

///
///
pub type Error {
  NoMatchFound(row: Int, col: Int, lexeme: String)
}

///
///
pub opaque type Lexer(a, mode) {
  Lexer(matchers: fn(mode) -> List(Matcher(a, mode)))
}

type State(a) {
  State(
    source: List(String),
    tokens: List(Token(a)),
    current: #(Int, Int, String),
    row: Int,
    col: Int,
  )
}

// LEXER CONSTRUCTORS ----------------------------------------------------------

///
///
pub fn simple(matchers: List(Matcher(a, Nil))) -> Lexer(a, Nil) {
  Lexer(fn(_) { matchers })
}

/// An `advanced` lexer is one that can change what matchers it uses based on the
/// current mode. This is useful for sophisticated lexers that might need to
/// handle things like interpolated strings or indentation-sensitive syntax.
///
pub fn advanced(matchers: fn(mode) -> List(Matcher(a, mode))) -> Lexer(a, mode) {
  Lexer(fn(mode) { matchers(mode) })
}

// MATCHER CONSTRUCTORS --------------------------------------------------------

/// Create a custom [`Matcher`](#Matcher) that will consume the input and produce
/// a token with the given value if it is `Ok` or return a `NoMatch` if it fails.
/// The first parameter is a function that takes the current lexeme and the
/// second parameter is a one-grapheme lookahead.
///
/// Matchers created with this convienence function cannot change the lexer's
/// mode or skip ahead to the next iteration without consuming the input.
///
pub fn keep(f: fn(String, String) -> Result(a, Nil)) -> Matcher(a, mode) {
  use mode, lexeme, lookahead <- Matcher

  f(lexeme, lookahead)
  |> result.map(Keep(_, mode))
  |> result.unwrap(NoMatch)
}

/// Create a custom [`Matcher`](#Matcher) that will consume the input and move
/// to the next iteration without producing a token if it is `True` or return a
/// `NoMatch` if it fails. The first parameter is a function that takes the
/// current lexeme and the second parameter is a one-grapheme lookahead.
///
/// Matchers created with this convienence function cannot change the lexer's
/// mode or skip ahead to the next iteration without consuming the input.
///
pub fn drop(f: fn(String, String) -> Bool) -> Matcher(a, mode) {
  use mode, lexeme, lookahead <- Matcher

  case f(lexeme, lookahead) {
    True -> Drop(mode)
    False -> NoMatch
  }
}

/// Create a custom [`Matcher`](#Matcher) that is flexible enough to do anything
/// you want! The first parameter is a function that takes the current lexer mode,
/// the current lexeme, and a one-grapheme lookahead.
///
/// The function returns a [`Match`](#Match) that tells the lexer what to do next.
///
pub fn custom(f: fn(mode, String, String) -> Match(a, mode)) -> Matcher(a, mode) {
  Matcher(f)
}

/// Take an existing matcher and transform it by applying a function to the value
/// it produces.
///
pub fn map(matcher: Matcher(a, mode), f: fn(a) -> b) -> Matcher(b, mode) {
  use mode, lexeme, lookahead <- Matcher

  case matcher.run(mode, lexeme, lookahead) {
    Keep(value, mode) -> Keep(f(value), mode)
    Skip(mode) -> Skip(mode)
    Drop(mode) -> Drop(mode)
    NoMatch -> NoMatch
  }
}

/// Take an existing matcher and transform it by applying a function to the value
/// it producs. The function you provide can return a different [`Match`](#Match)
/// so you can, for example, take a matcher that `Keep`s a value and turn it into
/// a matcher that `Drop`s the value instead. This is out [`ignore`](#ignore) works!
///
pub fn then(
  matcher: Matcher(a, mode),
  f: fn(a) -> Match(b, mode),
) -> Matcher(b, mode) {
  use mode, lexeme, lookahead <- Matcher

  case matcher.run(mode, lexeme, lookahead) {
    Keep(value, _) -> f(value)
    Skip(mode) -> Skip(mode)
    Drop(mode) -> Drop(mode)
    NoMatch -> NoMatch
  }
}

/// Take an existing matcher and transition to a new mode. This only runs if
/// the matcher is successful and either `Keep`s or `Drop`s a value.
///
///
pub fn into(matcher: Matcher(a, mode), f: fn(mode) -> mode) -> Matcher(a, mode) {
  use mode, lexeme, lookahead <- Matcher

  case matcher.run(mode, lexeme, lookahead) {
    Keep(value, mode) -> Keep(value, f(mode))
    Skip(mode) -> Skip(mode)
    Drop(mode) -> Drop(f(mode))
    NoMatch -> NoMatch
  }
}

/// Take a matcher than might `Keep` anything and silently `Drop` anything it
/// produces instead. This is useful for things like whitespace or comments
/// where you want to consume some input but you don't want to emit a token.
///
pub fn ignore(matcher: Matcher(a, mode)) -> Matcher(b, mode) {
  use mode, lexeme, lookahead <- Matcher

  case matcher.run(mode, lexeme, lookahead) {
    Keep(_, mode) -> Drop(mode)
    Skip(mode) -> Skip(mode)
    Drop(mode) -> Drop(mode)
    NoMatch -> NoMatch
  }
}

// COMMON MATCHERS -------------------------------------------------------------

/// Match exactly the given string with no lookahead and produce the given value.
///
pub fn token(str: String, value: a) -> Matcher(a, mode) {
  use mode, lexeme, _ <- Matcher

  case lexeme == str {
    True -> Keep(value, mode)
    False -> NoMatch
  }
}

/// Match exactly the given string only when the lookahead is matched by the given
/// breaker _regex_. This is an alias of [`keyword`](#keyword) but it can be
/// helpful to separate the two concepts.
///
pub fn symbol(str: String, breaker: String, value: a) -> Matcher(a, mode) {
  let assert Ok(break) = regexp.from_string(breaker)

  use mode, lexeme, lookahead <- Matcher

  case lexeme == str && { lookahead == "" || regexp.check(break, lookahead) } {
    True -> Keep(value, mode)
    False -> NoMatch
  }
}

/// Match exactly the given string only when the lookahead is matched by the given
/// breaker _regex_. Keywords are exact strings like `let` but you wouldn't want
/// to lex `letter` as `[Let, Var("tter")]` so the breaker is used so you can say
/// what characters should trigger a match.
///
pub fn keyword(str: String, breaker: String, value: a) -> Matcher(a, mode) {
  let assert Ok(break) = regexp.from_string(breaker)

  use mode, lexeme, lookahead <- Matcher

  case lexeme == str && { lookahead == "" || regexp.check(break, lookahead) } {
    True -> Keep(value, mode)
    False -> NoMatch
  }
}

///
///
pub fn int(to_value: fn(Int) -> a) -> Matcher(a, mode) {
  int_with_separator("", to_value)
}

///
///
pub fn int_with_separator(
  separator: String,
  to_value: fn(Int) -> a,
) -> Matcher(a, mode) {
  let assert Ok(digit) = regexp.from_string("[0-9" <> separator <> "]")
  let assert Ok(integer) = regexp.from_string("^[0-9" <> separator <> "]+$")

  use mode, lexeme, lookahead <- Matcher

  case !regexp.check(digit, lookahead) && regexp.check(integer, lexeme) {
    False -> NoMatch
    True -> {
      let assert Ok(num) =
        lexeme
        |> string.replace(separator, "")
        |> int.parse
      Keep(to_value(num), mode)
    }
  }
}

///
///
pub fn float(to_value: fn(Float) -> a) -> Matcher(a, mode) {
  float_with_separator("", to_value)
}

///
///
pub fn float_with_separator(
  separator: String,
  to_value: fn(Float) -> a,
) -> Matcher(a, mode) {
  let assert Ok(digit) = regexp.from_string("[0-9" <> separator <> "]")
  let assert Ok(integer) = regexp.from_string("^[0-9" <> separator <> "]+$")
  let assert Ok(number) =
    regexp.from_string(
      "^[0-9" <> separator <> "]+\\.[0-9" <> separator <> "]+$",
    )

  use mode, lexeme, lookahead <- Matcher
  let is_int = !regexp.check(digit, lookahead) && regexp.check(integer, lexeme)
  let is_float = !regexp.check(digit, lookahead) && regexp.check(number, lexeme)

  case lexeme {
    "." if is_int -> NoMatch

    _ if is_float -> {
      let assert Ok(num) =
        lexeme
        |> string.replace(separator, "")
        |> float.parse
      Keep(to_value(num), mode)
    }

    _ -> NoMatch
  }
}

pub fn number(
  from_int: fn(Int) -> a,
  from_float: fn(Float) -> a,
) -> Matcher(a, mode) {
  number_with_separator("", from_int, from_float)
}

pub fn number_with_separator(
  separator: String,
  from_int: fn(Int) -> a,
  from_float: fn(Float) -> a,
) -> Matcher(a, mode) {
  let assert Ok(digit) = regexp.from_string("[0-9" <> separator <> "]")
  let assert Ok(integer) = regexp.from_string("^[0-9" <> separator <> "]+$")
  let assert Ok(number) =
    regexp.from_string(
      "^[0-9" <> separator <> "]+\\.[0-9" <> separator <> "]+$",
    )

  use mode, lexeme, lookahead <- Matcher
  let is_int = !regexp.check(digit, lookahead) && regexp.check(integer, lexeme)
  let is_float = !regexp.check(digit, lookahead) && regexp.check(number, lexeme)

  case lexeme, lookahead {
    ".", _ if is_int -> NoMatch
    _, "." if is_int -> NoMatch

    _, _ if is_int -> {
      let assert Ok(num) =
        lexeme
        |> string.replace(separator, "")
        |> int.parse
      Keep(from_int(num), mode)
    }

    _, _ if is_float -> {
      let assert Ok(num) =
        lexeme
        |> string.replace(separator, "")
        |> float.parse
      Keep(from_float(num), mode)
    }

    _, _ -> NoMatch
  }
}

///
///
pub fn string(char: String, to_value: fn(String) -> a) -> Matcher(a, mode) {
  let assert Ok(is_string) =
    regexp.from_string(
      "^" <> char <> "([^" <> char <> "\\\\]|\\\\[\\s\\S])*" <> char <> "$",
    )
  use mode, lexeme, _ <- Matcher

  case regexp.check(is_string, lexeme) {
    True ->
      lexeme
      |> string.drop_start(1)
      |> string.drop_end(1)
      |> to_value
      |> Keep(mode)
    False -> NoMatch
  }
}

///
///
pub fn identifier(
  start: String,
  inner: String,
  reserved: Set(String),
  to_value: fn(String) -> a,
) -> Matcher(a, mode) {
  let assert Ok(ident) = regexp.from_string("^" <> start <> inner <> "*$")
  let assert Ok(inner) = regexp.from_string(inner)

  use mode, lexeme, lookahead <- Matcher

  case regexp.check(inner, lookahead), regexp.check(ident, lexeme) {
    True, True -> Skip(mode)
    False, True ->
      case set.contains(reserved, lexeme) {
        True -> NoMatch
        False -> Keep(to_value(lexeme), mode)
      }
    _, _ -> NoMatch
  }
}

///
///
pub fn try_identifier(
  start: String,
  inner: String,
  reserved: Set(String),
  to_value: fn(String) -> a,
) -> Result(Matcher(a, mode), regexp.CompileError) {
  use ident <- result.try(regexp.from_string("^" <> start <> inner <> "*$"))
  use inner <- result.map(regexp.from_string(inner))

  use mode, lexeme, lookahead <- Matcher

  case regexp.check(inner, lookahead), regexp.check(ident, lexeme) {
    True, True -> Skip(mode)
    False, True ->
      case set.contains(reserved, lexeme) {
        True -> NoMatch
        False -> Keep(to_value(lexeme), mode)
      }
    _, _ -> NoMatch
  }
}

///
///
pub fn variable(
  reserved: Set(String),
  to_value: fn(String) -> a,
) -> Matcher(a, mode) {
  identifier("[a-z]", "[a-zA-Z0-9_]", reserved, to_value)
}

///
///
pub fn spaces(token: a) -> Matcher(a, mode) {
  spaces_(fn(_) { token })
}

///
///
pub fn spaces_(to_value: fn(String) -> a) -> Matcher(a, mode) {
  let assert Ok(spaces) = regexp.from_string("^[ \\t]+")

  use mode, lexeme, _ <- Matcher

  case regexp.check(spaces, lexeme) {
    True -> Keep(to_value(lexeme), mode)
    False -> NoMatch
  }
}

///
///
pub fn whitespace(token: a) -> Matcher(a, mode) {
  let assert Ok(whitespace) = regexp.from_string("^\\s+$")

  use mode, lexeme, _ <- Matcher

  case regexp.check(whitespace, lexeme) {
    True -> Keep(token, mode)
    False -> NoMatch
  }
}

///
pub fn comment(start: String, to_value: fn(String) -> a) -> Matcher(a, mode) {
  let drop_length = string.length(start)
  use mode, lexeme, lookahead <- Matcher

  case string.starts_with(lexeme, start), lookahead {
    True, "\n" | True, "" ->
      lexeme
      |> string.drop_start(drop_length)
      |> to_value
      |> Keep(mode)
    True, _ -> Skip(mode)
    False, _ -> NoMatch
  }
}

// RUNNING A LEXER -------------------------------------------------------------

///
///
pub fn run(
  source: String,
  lexer: Lexer(a, Nil),
) -> Result(List(Token(a)), Error) {
  string.to_graphemes(source)
  |> State([], #(1, 1, ""), 1, 1)
  |> do_run(lexer, Nil, _)
}

///
///
pub fn run_advanced(
  source: String,
  mode: mode,
  lexer: Lexer(a, mode),
) -> Result(List(Token(a)), Error) {
  do_run(lexer, mode, State(string.to_graphemes(source), [], #(1, 1, ""), 1, 1))
}

fn do_run(
  lexer: Lexer(a, mode),
  mode: mode,
  state: State(a),
) -> Result(List(Token(a)), Error) {
  let matchers = lexer.matchers(mode)

  case state.source, state.current {
    // If we're at the end of the source and there's no lexeme left to match,
    // we're done!
    //
    // We have to remember to reverse the list of tokens because we've been building
    // it backwards using `[token, ..state.tokens]`. This is much quicker than
    // trying to prepend to the list as we go.
    [], #(_, _, "") -> Ok(list.reverse(state.tokens))

    // If we're at the end of the source but there's still a lexeme left to match,
    // we'll run the final `do_match` and return the result. If we get a `NoMatch`
    // at this point something went wrong.
    [], #(start_row, start_col, lexeme) ->
      case do_match(mode, lexeme, "", matchers) {
        NoMatch -> Error(NoMatchFound(start_row, start_col, lexeme))
        Skip(_) -> Error(NoMatchFound(start_row, start_col, lexeme))
        Drop(_) -> Ok(list.reverse(state.tokens))
        Keep(value, _) -> {
          let span = Span(start_row, start_col, state.row, state.col)
          let token = Token(span, lexeme, value)

          Ok(list.reverse([token, ..state.tokens]))
        }
      }

    // When lexing we include a one-grapheme lookahead to help us with things like
    // matching identifiers or other mode-aware tokens. This just takes the
    // skip grapheme from the source (we call it `lookahead` here) and calls the
    // `do_match` function with it and some other bits.
    [lookahead, ..rest], #(start_row, start_col, lexeme) -> {
      let row = next_row(state.row, lookahead)
      let col = next_col(state.col, lookahead)

      case do_match(mode, lexeme, lookahead, matchers) {
        Keep(value, mode) -> {
          let span = Span(start_row, start_col, state.row, state.col)
          let token = Token(span, lexeme, value)

          do_run(
            lexer,
            mode,
            State(
              source: rest,
              tokens: [token, ..state.tokens],
              current: #(state.row, state.col, lookahead),
              row: row,
              col: col,
            ),
          )
        }

        // A skip says that a matcher has matched the lexeme but still wants to
        // consume more input. This is mostly useful for things like identifiers
        // where the current lexeme is in the set of reserved words but we can
        // see the lookahead and know that it's not a reserved word.
        Skip(mode) ->
          do_run(
            lexer,
            mode,
            State(
              source: rest,
              tokens: state.tokens,
              current: #(start_row, start_col, lexeme <> lookahead),
              row: row,
              col: col,
            ),
          )

        // A drop says that we've matched the lexeme but we don't want to emit a
        // token. This is mostly useful for things like comments or whitespace that
        // users may not want to appear in the final token stream but do want to
        // handle in the lexer.
        Drop(mode) ->
          do_run(
            lexer,
            mode,
            State(
              source: rest,
              tokens: state.tokens,
              current: #(state.row, state.col, lookahead),
              row: row,
              col: col,
            ),
          )

        NoMatch ->
          do_run(
            lexer,
            mode,
            State(
              source: rest,
              tokens: state.tokens,
              current: #(start_row, start_col, lexeme <> lookahead),
              row: row,
              col: col,
            ),
          )
      }
    }
  }
}

fn do_match(
  mode: mode,
  str: String,
  lookahead: String,
  matchers: List(Matcher(a, mode)),
) -> Match(a, mode) {
  use _, matcher <- list.fold_until(matchers, NoMatch)

  case matcher.run(mode, str, lookahead) {
    Keep(_, _) as match -> list.Stop(match)
    Skip(_) as match -> list.Stop(match)
    Drop(_) as match -> list.Stop(match)
    NoMatch -> list.Continue(NoMatch)
  }
}

// UTILS -----------------------------------------------------------------------

fn next_col(col: Int, str: String) -> Int {
  case str {
    "\n" -> 1
    _ -> col + 1
  }
}

fn next_row(row: Int, str: String) -> Int {
  case str {
    "\n" -> row + 1
    _ -> row
  }
}
