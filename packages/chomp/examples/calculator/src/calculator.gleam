import chomp
import chomp/lexer
import chomp/pratt
import gleam/float
import gleam/int
import gleam/io
import gleam/option.{None, Some}
import gleam/result

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let math = "(1 + 3) * 4.5 + -1^3"

  let assert Ok(tokens) = lexer.run(math, lexer())
  let assert Ok(result) = chomp.run(tokens, expression())

  io.debug(result)
  //=> 17.0
}

// LEXER -----------------------------------------------------------------------

type Token {
  Number(Float)
  Plus
  Minus
  Times
  Divide
  Power
  LParen
  RParen
}

/// A lexer takes in a string and turns it into a flat list of tokens.
fn lexer() -> lexer.Lexer(Token, Nil) {
  lexer.simple([
    // Chomp has a built-in number lexer than accepts both ints and floats. Here,
    // we convert any ints to floats so that only a single number token, `Number`,
    // is needed.
    lexer.number(fn(x) { Number(int.to_float(x)) }, Number),
    // These tokens are single characters.
    lexer.token("+", Plus),
    lexer.token("-", Minus),
    lexer.token("*", Times),
    lexer.token("/", Divide),
    lexer.token("^", Power),
    lexer.token("(", LParen),
    lexer.token(")", RParen),
    // Whitespace is ignored so we don't have to deal with it in the parser.
    lexer.whitespace(Nil) |> lexer.ignore,
  ])
}

// PARSER ----------------------------------------------------------------------

/// Since the `chomp.Parser` type has 4 type parameters, a type alias such as
/// this one is very helpful for keeping the code more readable.
type Parser(a) =
  chomp.Parser(a, String, Token, Nil)

/// This is the main parser function. A parser transforms a list of tokens into
/// anything you want—in this case, a float.
fn expression() -> Parser(Float) {
  pratt.expression(
    // An operand is either a number or a parenthesized expression and can be
    // negated.
    one_of: [
      fn(_) { take_number() },
      fn(_) { parentheses() },
      pratt.prefix(4, chomp.token(Minus), float.negate),
    ],
    // This error is thrown if no operand could be parsed.
    or_error: "Expected a primary expression",
    // There are four operators, each of which is left-associative. The number
    // within the parentheses is the precedence of the operator. Since multiplication
    // and division have higher precedence than addition and subtraction, their
    // precedence number is larger (2 vs. 1).
    and_then: [
      pratt.infix(pratt.Left(1), chomp.token(Plus), float.add),
      pratt.infix(pratt.Left(1), chomp.token(Minus), float.subtract),
      pratt.infix(pratt.Left(2), chomp.token(Times), float.multiply),
      pratt.infix(pratt.Left(2), chomp.token(Divide), fn(x, y) { x /. y }),
      pratt.infix(pratt.Right(3), chomp.token(Power), fn(x, y) {
        float.power(x, y) |> result.unwrap(0.0)
      }),
    ],
  )
}

/// This parser accepts a `Number` token and returns the float within it.
fn take_number() -> Parser(Float) {
  chomp.take_map(fn(tok) {
    case tok {
      Number(n) -> Some(n)
      _ -> None
    }
  })
}

fn parentheses() -> Parser(Float) {
  use _ <- chomp.do(chomp.token(LParen))
  use inner <- chomp.do(expression())
  use _ <- chomp.do(chomp.token(RParen))
  chomp.return(inner)
}
