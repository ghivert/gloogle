# chomp

A lexer and parser combinator library inspired by [`elm/parser`](https://github.com/elm/parser).

> This package is a fork of [nibble](https://github.com/hayleigh-dot-dev/gleam-nibble) that does some things differently. Most notably, error dead ends are removed (instead favoring new functions for updating errors), and custom errors can be thrown. Some parsers have also been removed or modified and others added.
>
> If coming from nibble, be aware that things may not work the same!

[![Package Version](https://img.shields.io/hexpm/v/chomp)](https://hex.pm/packages/chomp)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/chomp/)

✨ This project is written in **pure Gleam** so you can use it with both the Erlang and JavaScript targets.

## Quick start

If you just want to get a feel for what chomp can do, check out the example
below.

```gleam
import gleam/option.{None, Some}
import chomp.{do, return}
import chomp/lexer

type Point {
  Point(x: Int, y: Int)
}

type Token {
  Num(Int)
  LParen
  RParen
  Comma
}

pub fn main() {
  // Your lexer knows how to take an input string and
  // turn it into a flat list of tokens. You define the
  // type of token you want to use, but chomp will wrap
  // that up in its own `Token` type that includes the
  // source span and original lexeme for each token.
  let lexer =
    lexer.simple([
      lexer.int(Num),
      lexer.token("(", LParen),
      lexer.token(")", RParen),
      lexer.token(",", Comma),
      // Skip over whitespace, we don't care about it!
      lexer.whitespace(Nil)
        |> lexer.ignore,
    ])

  // Your parser(s!) know how to transform a list of
  // tokens into whatever you want. You have the full
  // power of Gleam here, so you can go wild!
  let int_parser =
    // Use `take_map` to only consume certain kinds of tokens and transform the
    // result.
    chomp.take_map(fn(tok) {
      case tok {
        Num(n) -> Some(n)
        _ -> None
      }
    })
    |> chomp.or_error("expected a number")

  let parser = {
    use _ <- do(chomp.token(LParen))
    use x <- do(int_parser)
    use _ <- do(chomp.token(Comma))
    use y <- do(int_parser)
    use _ <- do(chomp.token(RParen))

    return(Point(x, y))
  }

  let assert Ok(tokens) = lexer.run("(1, 2)", lexer)
  let assert Ok(point) = chomp.run(tokens, parser)

  point.x //=> 1
  point.y //=> 2
}

```

## Examples

Chomp does not currently have a tutorial or guide, so the best way to learn is through the docs and [examples](./examples/). Feel free to submit a PR if you have a short, well-written parser that you think would serve as a good example!

## Installation

This package can be added to your Gleam project:

```sh
gleam add chomp
```

and its documentation can be found at <https://hexdocs.pm/chomp>.
