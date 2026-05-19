// IMPORTS ---------------------------------------------------------------------

import chomp/lexer.{type Token, Token}
import chomp/span.{type Span, Span}
import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}

// TYPES -----------------------------------------------------------------------

/// The `Parser` type has four parameters; let's take a look at each of them:
///
/// ```
/// Parser(a, e, tok, ctx)
/// ```
///
/// 1) `a` is the type of value that the parser knows how to produce. If you were
///   writing a parser for a programming language, this might be your expression
///   type.
///
/// 2) `e` is the type of custom error that you can choose to throw. This could
///   be a `String` for simple errors such as "I expected an expression" or
///   something more complex.
///
/// 3) `tok` is the type of tokens that the parser knows how to consume. You can
///   take a look at the [`Token`](./chomp/lexer#Token) type for a bit more info,
///   but note that it's not necessary for the token stream to come from chomp's
///   lexer.
///
/// 4) `ctx` is used to make error reporting nicer. You can place a parser into a
///   custom context. When the parser runs the context gets pushed into a stack.
///   If the parser fails you can see the context stack in the error message,
///   which can make error reporting and debugging much easier! See the [`in`](#in)
///   function for more details.
///
/// It can get a bit repetitive to write out the `Parser` type with all of the
/// parameters in type annotations, so it's common to make a type alias for it in
/// your parser.
///
/// ```
/// import chomp
///
/// type Parser(a) = chomp.Parser(a, MyErrorType, TokenType, Context)
///
/// // now your parsers can use the alias like so:
/// fn expression() -> Parser(Expression) {
///   ...
/// }
/// ```
///
pub opaque type Parser(a, e, tok, ctx) {
  Parser(fn(State(tok, ctx)) -> Step(a, e, tok, ctx))
}

type Step(a, e, tok, ctx) {
  Cont(Committed, a, State(tok, ctx))
  Fail(Committed, problem: Error(e, tok), pos: Span, ctx: List(#(Span, ctx)))
}

type State(tok, ctx) {
  State(
    // The Gleam stdlib doesn't seem to have an `Array` type, so we'll just
    // use a `Dict` instead. We only need something for indexed access, to it's
    // not a huge deal.
    //
    // TODO: Louis says making an `Array` backed by tuples in Erlang will
    // be way better for performance. In JavaScript we could just use normal
    // arrays - someone should look into this.
    //
    // ❓ You might wonder why we're wanting an `Array` at all when we could just
    // use a `List` and backtrack to a previous state when we need to. By tracking
    // the index and indexing into the dict/array directly we save ever having to
    // allocate something new, which is a big deal for performance!
    src: Dict(Int, Token(tok)),
    idx: Int,
    pos: Span,
    ctx: List(#(Span, ctx)),
  )
}

/// Is a parser *committed* or not? That sounds pretty confusing, but in general
/// a parser commits when it has consumed at least one token.
///
type Committed {
  Committed(Bool)
}

/// The `Error` type represents all of the ways that a parser can fail. It has
/// two type parameters, `e` and `tok`. See the [`Parser`](#Parser) type for more
/// information about them.
///
pub type Error(e, tok) {
  /// A custom error.
  Custom(e)
  /// There are no more tokens to consume, but the parser required some.
  EndOfInput
  /// The parser expected a certain token but got a different one instead.
  Expected(tok, got: tok)
  /// The parser encountered an unexpected token. This error is not very specific,
  /// so it's often best to replace it using the [`or_error`](#or_error) function
  /// when possible.
  Unexpected(tok)
  /// A parser was called with incorrect input.
  BadParser(String)
}

// RUNNING PARSERS -------------------------------------------------------------

/// Parsers don't do anything until they're run! The `run` function takes a
/// [`Parser`](#Parser) and a list of [`Token`](./chomp/lexer#Token)s and
/// runs it; returning either the parsed value or a tuple of the [`Error`](#Error)
/// with the position where the parser failed and the final context stack.
///
pub fn run(
  src: List(Token(tok)),
  parser: Parser(a, e, tok, ctx),
) -> Result(a, #(Error(e, tok), Span, List(#(Span, ctx)))) {
  let src =
    list.index_fold(src, dict.new(), fn(dict, tok, idx) {
      dict.insert(dict, idx, tok)
    })
  let init = State(src, 0, Span(1, 1, 1, 1), [])

  case runwrap(init, parser) {
    Cont(_, a, _) -> Ok(a)
    Fail(_, problem, pos, ctx) -> Error(#(problem, pos, ctx))
  }
}

fn runwrap(
  state: State(tok, ctx),
  parser: Parser(a, e, tok, ctx),
) -> Step(a, e, tok, ctx) {
  let Parser(parse) = parser
  parse(state)
}

fn next(state: State(tok, ctx)) -> #(Option(tok), State(tok, ctx)) {
  case dict.get(state.src, state.idx) {
    Error(_) -> #(option.None, state)
    Ok(Token(span, _, tok)) -> #(
      option.Some(tok),
      State(..state, idx: state.idx + 1, pos: span),
    )
  }
}

// CONSTRUCTORS ----------------------------------------------------------------

/// The simplest kind of parser. [`return`](#return) consumes no tokens and always
/// produces the given value. Sometimes called [`succeed`](#succeed) instead.
///
/// This function might seem useless at first, but it is very useful when used in
/// combination with [`do`](#do) or [`then`](#then).
///
/// ```gleam
/// import chomp.{do, return}
///
/// fn unit8_parser() {
///   use int <- do(int_parser())
///
///   case int >= 0, int <= 255 {
///     True, True ->
///       return(int)
///
///     False, _ ->
///       throw("Expected an int >= 0")
///
///     _, False ->
///       throw("Expected an int <= 255")
///  }
/// }
/// ```
///
/// 💡 [`return`](#return) and [`succeed`](#succeed) are names for the same thing.
/// We suggesting using `return` unqualified when using `do` and Gleam's `use`
/// syntax, and `chomp.succeed` in a pipeline with `chomp.then`.
///
pub fn return(value: a) -> Parser(a, e, tok, ctx) {
  use state <- Parser

  Cont(Committed(False), value, state)
}

///💡 [`succeed`](#succeed) and [`return`](#return) are names for the same thing.
/// We suggest using `succeed` in a pipeline with `chomp.then`, and `return`
/// unqalified when using `do` with Gleam's `use` syntax.
///
pub fn succeed(value: a) -> Parser(a, e, tok, ctx) {
  return(value)
}

/// The opposite of [`return`](#return), this parser always fails with the given
/// error. Sometimes called [`fail`](#fail) instead.
///
pub fn throw(error: e) -> Parser(a, e, tok, ctx) {
  use state <- Parser
  let error = Custom(error)

  Fail(Committed(False), error, state.pos, state.ctx)
}

/// Create a parser that consumes no tokens and always fails with the given
/// error.
///
pub fn fail(error: e) -> Parser(a, e, tok, ctx) {
  throw(error)
}

/// Defer the creation of a parser until it is needed. This is often most useful
/// when creating a parser that is recursive and is *not* a function.
///
pub fn lazy(parser: fn() -> Parser(a, e, tok, ctx)) -> Parser(a, e, tok, ctx) {
  use state <- Parser

  runwrap(state, parser())
}

// BACKTRACKING ----------------------------------------------------------------

/// By default, parsers will not backtrack if they fail after consuming at least
/// one token. Passing a parser to `backtrackable` will change this behaviour and
/// allows us to jump back to the state of the parser before it consumed any input
/// and try another one.
///
/// This is most useful when you want to quickly try a few different parsers using
/// [`one_of`](#one_of).
///
/// 🚨 Backtracking parsers can drastically reduce performance, so you should avoid
/// them where possible. A common reason folks reach for backtracking is when they
/// want to try multiple branches that start with the same token or same sequence
/// of tokens.
///
/// To avoid backtracking in these cases, you can create an intermediate parser
/// that consumes the common tokens _and then_ use [`one_of`](#one_of) to try
/// the different branches.
///
pub fn backtrackable(parser: Parser(a, e, tok, ctx)) -> Parser(a, e, tok, ctx) {
  use state <- Parser

  case runwrap(state, parser) {
    Cont(_, a, state) -> Cont(Committed(False), a, state)
    Fail(_, problem, pos, ctx) -> Fail(Committed(False), problem, pos, ctx)
  }
}

fn should_commit(a: Committed, or b: Committed) -> Committed {
  let Committed(a) = a
  let Committed(b) = b

  Committed(a || b)
}

// MANIPULATING PARSERS --------------------------------------------------------

/// Do the first `parser`, then apply `f` to its result, which returns another
/// parser and is subsequently run. If the first parser fails, `f` is not called.
/// Because this parser is so common, we normally import it unqualified.
///
/// This is very useful for running parsers in sequence with Gleam's `use` syntax.
///
/// ```
/// import chomp.{do, return}
///
/// fn point() {
///   use _ <- do(chomp.token(LParen))
///   use x <- do(int_parser())
///   use _ <- do(chomp.token(Comma))
///   use y <- do(int_parser())
///   use _ <- do(chomp.token(RParen))
///
///   return(Point(x, y))
/// }
/// ```
///
/// (See the [main page](https://hexdocs.pm/chomp) for a complete example)
///
pub fn do(
  parser: Parser(a, e, tok, ctx),
  f: fn(a) -> Parser(b, e, tok, ctx),
) -> Parser(b, e, tok, ctx) {
  use state <- Parser

  case runwrap(state, parser) {
    Cont(to_a, a, state) ->
      case runwrap(state, f(a)) {
        Cont(to_b, b, state) -> Cont(should_commit(to_a, or: to_b), b, state)
        Fail(to_b, problem, pos, ctx) ->
          Fail(should_commit(to_a, or: to_b), problem, pos, ctx)
      }
    Fail(committed, problem, pos, ctx) -> Fail(committed, problem, pos, ctx)
  }
}

/// A combination of [`do`](#do) and [`in`](#in).
///
pub fn do_in(
  context: ctx,
  parser: Parser(a, e, tok, ctx),
  f: fn(a) -> Parser(b, e, tok, ctx),
) -> Parser(b, e, tok, ctx) {
  do(parser, f)
  |> in(context)
}

/// Another name for [`do`](#do).
///
pub fn then(
  parser: Parser(a, e, tok, ctx),
  f: fn(a) -> Parser(b, e, tok, ctx),
) -> Parser(b, e, tok, ctx) {
  do(parser, f)
}

/// Run `parser`, applying `f` to its result.
///
pub fn map(
  parser: Parser(a, e, tok, ctx),
  f: fn(a) -> b,
) -> Parser(b, e, tok, ctx) {
  use a <- do(parser)

  return(f(a))
}

/// Run `parser`, replacing its result with `b`.
///
pub fn replace(
  parser: Parser(a, e, tok, ctx),
  with b: b,
) -> Parser(b, e, tok, ctx) {
  map(parser, fn(_) { b })
}

/// Run a parser and if it fails and did not consume any tokens, return the given
/// error. This function is extremely useful for providing custom error messages
/// to parsers such as `one_of`, `take_map`, and `take_if`.
///
/// Note that `EndOfInput` errors are not replaced to minimize confusing error
/// messages.
///
/// ```gleam
/// import chomp
///
/// fn value() {
///   chomp.one_of([
///     string(),
///     number(),
///     variable(),
///     list(),
///     group(),
///   ])
///   |> chomp.or_error("I expected a value (string, number, variable, or list)")
/// }
/// ```
///
pub fn or_error(
  parser: Parser(a, e, tok, ctx),
  error: e,
) -> Parser(a, e, tok, ctx) {
  map_error(parser, fn(_) { Custom(error) })
}

/// Run a parser and if it fails and did not consume any tokens, apply the given
/// function to the error and return the new error. This function provides a
/// way to add more information to error messages. For example, if you just parsed
/// some indentation in an indentation-sensitive language and now want to parse
/// a statement, you could map the statement parser's error to add extra info:
///
/// ```gleam
/// fn parse_statement() {
///   // parse a statement
///   |> chomp.or_error("I wanted a statement")
/// }
///
/// fn parse_something() {
///   use _ <- do(parse_indentation())
///   use statement <- do(
///     parse_statement() |> chomp.map_error(because_of_indent)
///   )
///   // ...
/// }
///
/// fn because_of_indent(error) {
///   case error {
///     chomp.Custom(error) ->
///       chomp.Custom("Since there was an indent, " <> error)
///     _ -> error
///   }
/// }
/// ```
///
/// Now if parsing the statement in `parse_something` fails, the error message
/// will be `Since there was an indent, I wanted a statement`. Sweet!
///
/// Note that, like `or_error`, `EndOfInput` errors are not changed to minimize
/// confusing error messages.
///
pub fn map_error(
  parser: Parser(a, e, tok, ctx),
  map: fn(Error(e, tok)) -> Error(e, tok),
) -> Parser(a, e, tok, ctx) {
  use state <- Parser

  case runwrap(state, parser) {
    Fail(_, EndOfInput, _, _) as fail -> fail
    Fail(Committed(False), error, pos, ctx) ->
      Fail(Committed(False), map(error), pos, ctx)
    result -> result
  }
}

/// Run a parser and if it fails, return the given error. This parser is similar
/// to [`or_error`](#or_error), but it's not quite the same—this one does not
/// care whether the parser consumed any tokens when it failed. That means that
/// if you have a parser like this:
///
/// ```
/// chomp.one_of([function(), constant()])
/// |> chomp.replace_error("I expected a function or constant")
/// ```
///
/// You will always get the error message "I expected a function or constant" even
/// if any of the `one_of` parsers consumed tokens—a behavior that probably isn't
/// what you want.
///
/// For instance, if this was parsed:
///
/// ```js
/// function 23() {}
/// //       ^^ this is the error
/// ```
///
/// We would get the error message "I expected a function or constant" because the
/// `function` parser's error was swallowed by `replace_error`. Contrast that to
/// `or_error`, where we would get something much better such as "I expected an
/// identifier".
///
/// Of course there *are* cases where `replace_error` can be helpful; just be
/// aware that you'll often want to reach for `or_error` first.
///
pub fn replace_error(
  parser: Parser(a, e, tok, ctx),
  error: e,
) -> Parser(a, e, tok, ctx) {
  use state <- Parser

  case runwrap(state, parser) {
    Fail(committed, _, pos, ctx) -> Fail(committed, Custom(error), pos, ctx)
    result -> result
  }
}

// PARSER STATE ----------------------------------------------------------------

/// A parser that returns the current token position.
///
pub fn get_pos() -> Parser(Span, e, tok, ctx) {
  use state <- Parser

  Cont(Committed(False), state.pos, state)
}

/// Returns `True` if there are no tokens left to consume.
///
pub fn is_at_end() -> Parser(Bool, e, tok, ctx) {
  use state <- Parser

  case next(state) {
    #(option.Some(_), _) -> Cont(Committed(False), False, state)
    #(option.None, _) -> Cont(Committed(False), True, state)
  }
}

// SIMPLE PARSERS --------------------------------------------------------------

/// Parse a single token of any type and return it.
///
pub fn any() -> Parser(tok, e, tok, ctx) {
  take_if(fn(_) { True })
}

/// Parse a token of a particular type, returning its position.
///
/// ```
/// use start_pos <- do(chomp.token(LParen))
/// // ...
/// use end_pos <- do(chomp.token(RParen))
/// ```
///
pub fn token(tok: tok) -> Parser(Span, e, tok, ctx) {
  use state <- Parser

  case next(state) {
    #(option.Some(t), state) if tok == t ->
      Cont(Committed(True), state.pos, state)
    #(option.Some(t), state) ->
      Fail(Committed(False), Expected(tok, t), state.pos, state.ctx)
    #(option.None, state) ->
      Fail(Committed(False), EndOfInput, state.pos, state.ctx)
  }
}

/// Parse successfully only if at the end of the token stream.
///
pub fn end() -> Parser(Nil, e, tok, ctx) {
  use state <- Parser

  case next(state) {
    #(option.Some(tok), state) ->
      Fail(Committed(False), Unexpected(tok), state.pos, state.ctx)
    #(option.None, _) -> Cont(Committed(False), Nil, state)
  }
}

// BRANCHING AND LOOPING -------------------------------------------------------

/// Try each parser in order until one succeeds. If none succeed, the last parser's
/// error is returned. It is recommended to use a custom error with
/// [`or_error`](#or_error) for better error messages.
///
pub fn one_of(parsers: List(Parser(a, e, tok, ctx))) -> Parser(a, e, tok, ctx) {
  use state <- Parser

  case parsers {
    [] ->
      Fail(
        Committed(False),
        BadParser("one_of requires at least one parser"),
        state.pos,
        state.ctx,
      )
    [parser] -> runwrap(state, parser)
    [parser, ..rest] ->
      case runwrap(state, parser) {
        Cont(..) as result -> result
        Fail(Committed(True), ..) as result -> result
        Fail(..) -> runwrap(state, one_of(rest))
      }
  }
}

/// Try the given parser, but if it fails return the given default value instead
/// of failing.
///
pub fn or(parser: Parser(a, e, tok, ctx), default: a) -> Parser(a, e, tok, ctx) {
  one_of([parser, return(default)])
}

/// Try the given parser, but if it fails return
/// [`None`](#https://hexdocs.pm/gleam_stdlib/gleam/option.html#Option) instead
/// of failing.
///
pub fn optional(
  parser: Parser(a, e, tok, ctx),
) -> Parser(Option(a), e, tok, ctx) {
  one_of([map(parser, Some), return(None)])
}

/// Parse a sequence separated by the `separator` parser.
///
pub fn sequence(
  parser: Parser(a, e, tok, ctx),
  separator sep: Parser(x, e, tok, ctx),
) -> Parser(List(a), e, tok, ctx) {
  one_of([
    parser
      |> then(more(_, parser, sep)),
    return([]),
  ])
}

/// Parse zero or more of the given parser.
///
pub fn many(parser: Parser(a, e, tok, ctx)) -> Parser(List(a), e, tok, ctx) {
  sequence(parser, return(Nil))
}

/// Parse one or more of the given parser.
///
/// 💡 If this parser succeeds, the list produced is guaranteed to be non-empty.
/// Feel free to `let assert` the result!
///
pub fn many1(parser: Parser(a, e, tok, ctx)) -> Parser(List(a), e, tok, ctx) {
  use x <- do(parser)
  use xs <- do(many(parser))

  return([x, ..xs])
}

/// Parse until the end of the token stream, returning a list of the results.
///
/// 💡 This parser produces better error messages than using both [`many`](#many)
/// and [`end`](#end) in order, so you'll often want to use this function instead.
/// A common use-case is when you're parsing top-level statements or expressions
/// in a programming language until the end of the file.
///
/// ```
/// use statements <- do(chomp.until_end(statement()))
/// return(statements)
///
/// // is generally better than
///
/// use statements <- do(chomp.many(statement()))
/// use _ <- do(chomp.end())
/// return(statements)
/// ```
///
pub fn until_end(parser: Parser(a, e, tok, ctx)) -> Parser(List(a), e, tok, ctx) {
  use xs <- loop([])
  use is_at_end <- do(is_at_end())

  case is_at_end {
    True -> return(Break(list.reverse(xs)))
    False -> {
      use x <- do(parser)
      return(Continue([x, ..xs]))
    }
  }
}

/// Parse until the given token is encountered, returning a list of the results.
///
pub fn until(
  parser: Parser(a, e, tok, ctx),
  tok: tok,
) -> Parser(List(a), e, tok, ctx) {
  use xs <- loop([])

  let break =
    take_map(fn(t) {
      case t == tok {
        True -> Some(Break(list.reverse(xs)))
        False -> None
      }
    })

  let continue = {
    use x <- do(parser)
    return(Continue([x, ..xs]))
  }

  one_of([break, continue])
}

fn more(
  x: a,
  parser: Parser(a, e, tok, ctx),
  separator: Parser(x, e, tok, ctx),
) -> Parser(List(a), e, tok, ctx) {
  use xs <- loop([x])
  // `break` is lazy so we don't reverse `xs` every iteration if we don't need
  // to.
  let break = fn() { return(Break(list.reverse(xs))) }
  let continue = {
    use _ <- do(separator)
    use x <- do(parser)

    return(Continue([x, ..xs]))
  }

  one_of([continue, lazy(break)])
}

/// This type is very similar to
/// [`list.ContinueOrStop`](https://hexdocs.pm/gleam_stdlib/gleam/list.html#ContinueOrStop).
///
pub type Loop(a, state) {
  /// Continue parsing with the new state.
  Continue(state)
  /// Stop parsing and return a result.
  Break(a)
}

///
///
pub fn loop(
  init: state,
  step: fn(state) -> Parser(Loop(a, state), e, tok, ctx),
) -> Parser(a, e, tok, ctx) {
  use state <- Parser

  loop_help(step, Committed(False), init, state)
}

fn loop_help(f, commit, loop_state, state) {
  case runwrap(state, f(loop_state)) {
    Cont(can_backtrack, Continue(next_loop_state), next_state) ->
      loop_help(
        f,
        should_commit(commit, can_backtrack),
        next_loop_state,
        next_state,
      )
    Cont(can_backtrack, Break(result), next_state) ->
      Cont(should_commit(commit, can_backtrack), result, next_state)
    Fail(can_backtrack, problem, pos, ctx) ->
      Fail(should_commit(commit, can_backtrack), problem, pos, ctx)
  }
}

// PREDICATES ------------------------------------------------------------------

/// Take and return a token if it satisfies the predicate.
///
pub fn take_if(predicate: fn(tok) -> Bool) -> Parser(tok, e, tok, ctx) {
  use state <- Parser
  let #(tok, state) = next(state)

  case tok, option.map(tok, predicate) {
    Some(tok), Some(True) -> Cont(Committed(True), tok, state)
    Some(tok), Some(False) ->
      Fail(Committed(False), Unexpected(tok), state.pos, state.ctx)
    _, _ -> Fail(Committed(False), EndOfInput, state.pos, state.ctx)
  }
}

/// Take the next token and attempt to transform it with the given function. This
/// is useful when creating reusable primtive parsers for your own tokens such as
/// `take_identifier` or `take_number`.
///
pub fn take_map(f: fn(tok) -> Option(a)) -> Parser(a, e, tok, ctx) {
  use state <- Parser
  let #(tok, state) = next(state)

  case tok, option.then(tok, f) {
    None, _ -> Fail(Committed(False), EndOfInput, state.pos, state.ctx)
    Some(tok), None ->
      Fail(Committed(False), Unexpected(tok), state.pos, state.ctx)
    _, Some(a) -> Cont(Committed(True), a, state)
  }
}

// CONTEXT ---------------------------------------------------------------------

/// Run a parser in a certain context. This allows you to include useful
/// information—*context*—in error messages. For example, instead of a message
/// such as "I expected a value" you could say "I expected a value inside of this
/// list in the function `foo`". The latter is far more user-friendly!
///
/// Context also holds on to the position of the parser where it was entered.
/// So if you had an `InList` context entered right after a `[` token, any errors
/// encountered inside the list can reference where the list started.
///
/// ```txt
/// I found a problem in this list:
///
/// | the list starts here
/// v
/// [a, b, ]
///        ~
///        |
/// I wanted a value
/// ```
///
pub fn in(
  parser: Parser(a, e, tok, ctx),
  context: ctx,
) -> Parser(a, e, tok, ctx) {
  use state <- Parser

  case runwrap(push_context(state, context), parser) {
    Cont(committed, a, state) -> Cont(committed, a, pop_context(state))
    Fail(committed, problem, pos, ctx) -> Fail(committed, problem, pos, ctx)
  }
}

fn push_context(state: State(tok, ctx), context: ctx) -> State(tok, ctx) {
  State(..state, ctx: [#(state.pos, context), ..state.ctx])
}

fn pop_context(state: State(tok, ctx)) -> State(tok, ctx) {
  case state.ctx {
    [] -> state
    [_, ..context] -> State(..state, ctx: context)
  }
}

/// Run the given parser and then inspect it's state.
pub fn inspect(
  parser: Parser(a, e, tok, ctx),
  message: String,
) -> Parser(a, e, tok, ctx) {
  use state <- Parser
  io.println(message <> ": ")

  runwrap(state, parser)
  |> echo
}
