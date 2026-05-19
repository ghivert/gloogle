//// This module is very similar to the
//// [`elm-pratt-parser`](https://package.elm-lang.org/packages/dmy/elm-pratt-parser/latest/Pratt)
//// package in the Elm ecosystem, and much of its docs apply to this module.

// IMPORTS ---------------------------------------------------------------------

import chomp.{type Parser}
import gleam/list

// TYPES -----------------------------------------------------------------------

pub opaque type Config(a, e, tok, ctx) {
  Config(
    one_of: List(fn(Config(a, e, tok, ctx)) -> Parser(a, e, tok, ctx)),
    or_error: e,
    and_then_one_of: List(Operator(a, e, tok, ctx)),
  )
}

pub opaque type Operator(a, e, tok, ctx) {
  Operator(
    fn(Config(a, e, tok, ctx)) -> #(Int, fn(a) -> Parser(a, e, tok, ctx)),
  )
}

pub type Precedence {
  Left(Int)
  Right(Int)
}

// PRATT PARSING ---------------------------------------------------------------

pub fn expression(
  one_of first: List(fn(Config(a, e, tok, ctx)) -> Parser(a, e, tok, ctx)),
  or_error or_error: e,
  and_then then: List(Operator(a, e, tok, ctx)),
) -> Parser(a, e, tok, ctx) {
  let config = Config(first, or_error, then)
  sub_expression(config, 0)
}

pub fn sub_expression(
  config: Config(a, e, tok, ctx),
  precedence: Int,
) -> Parser(a, e, tok, ctx) {
  let expr = {
    use <- chomp.lazy
    config.one_of
    |> list.map(fn(p) { p(config) })
    |> chomp.one_of
    |> chomp.or_error(config.or_error)
  }

  let go = fn(expr) {
    chomp.one_of([
      operation(expr, config, precedence)
        |> chomp.map(chomp.Continue),
      chomp.return(expr)
        |> chomp.map(chomp.Break),
    ])
  }

  use e <- chomp.do(expr)

  chomp.loop(e, go)
}

fn operation(
  expr: a,
  config: Config(a, e, tok, ctx),
  current_precedence: Int,
) -> Parser(a, e, tok, ctx) {
  config.and_then_one_of
  |> list.filter_map(fn(operator) {
    let Operator(op) = operator
    case op(config) {
      #(precedence, parser) if precedence > current_precedence ->
        Ok(parser(expr))

      _ -> Error(Nil)
    }
  })
  |> chomp.one_of
}

//

pub fn prefix(
  precedence: Int,
  operator: Parser(x, e, tok, ctx),
  apply: fn(a) -> a,
) -> fn(Config(a, e, tok, ctx)) -> Parser(a, e, tok, ctx) {
  prefix_custom(precedence, fn(_) { operator }, fn(a, _) { apply(a) })
}

pub fn prefix_custom(
  precedence: Int,
  operator: fn(Config(a, e, tok, ctx)) -> Parser(x, e, tok, ctx),
  apply: fn(a, x) -> a,
) -> fn(Config(a, e, tok, ctx)) -> Parser(a, e, tok, ctx) {
  fn(config) {
    use op <- chomp.do(operator(config))
    use subexpr <- chomp.do(sub_expression(config, precedence))

    chomp.return(apply(subexpr, op))
  }
}

pub fn infix(
  precedence: Precedence,
  operator: Parser(x, e, tok, ctx),
  apply: fn(a, a) -> a,
) -> Operator(a, e, tok, ctx) {
  infix_custom(precedence, fn(_) { operator }, fn(a, b, _) { apply(a, b) })
}

pub fn infix_custom(
  precedence: Precedence,
  operator: fn(Config(a, e, tok, ctx)) -> Parser(x, e, tok, ctx),
  apply: fn(a, a, x) -> a,
) -> Operator(a, e, tok, ctx) {
  let #(left_precedence, right_precedence) = case precedence {
    Left(p) -> #(p, p)
    Right(p) -> #(p, p - 1)
  }
  use config <- Operator
  #(left_precedence, fn(lhs) {
    use op <- chomp.do(operator(config))
    use subexpr <- chomp.do(sub_expression(config, right_precedence))

    chomp.return(apply(lhs, subexpr, op))
  })
}

pub fn postfix(
  precedence: Int,
  operator: Parser(x, e, tok, ctx),
  apply: fn(a) -> a,
) -> Operator(a, e, tok, ctx) {
  postfix_custom(precedence, fn(_) { operator }, fn(a, _) { apply(a) })
}

pub fn postfix_custom(
  precedence: Int,
  operator: fn(Config(a, e, tok, ctx)) -> Parser(x, e, tok, ctx),
  apply: fn(a, x) -> a,
) -> Operator(a, e, tok, ctx) {
  use config <- Operator
  #(precedence, fn(lhs) {
    use op <- chomp.do(operator(config))
    chomp.return(apply(lhs, op))
  })
}

pub fn operator_custom(
  build: fn(Config(a, e, tok, ctx)) -> #(Int, fn(a) -> Parser(a, e, tok, ctx)),
) {
  Operator(build)
}
