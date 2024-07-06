import chomp.{do, return}
import chomp/lexer
import chomp/span
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/result
import glexer
import glexer/token

pub type Kind {
  Index(String, Int)
  Custom(String, List(Kind))
  Function(List(Kind), Kind)
  Tuple(List(Kind))
}

fn parse_qualified_name() {
  parse_upper_name()
  |> chomp.then(fn(content) {
    use content_ <- do(chomp.optional(parse_type_parameter()))
    let assert Custom(c, _) = content
    chomp.succeed(Custom(c, option.unwrap(content_, [])))
  })
}

fn parse_upper_name() {
  use _ <- do({
    parse_name()
    |> chomp.sequence(chomp.token(token.Slash))
    |> chomp.optional
  })
  use _ <- do(chomp.optional(chomp.token(token.Dot)))
  use token <- chomp.take_map()
  case token {
    token.UpperName(content) -> Some(Custom(content, []))
    _ -> None
  }
}

fn parse_name() {
  use token <- chomp.take_map()
  case token {
    token.Name(content) -> Some(Index(content, 0))
    _ -> None
  }
}

fn parse_label() {
  chomp.backtrackable({
    use name <- do(
      chomp.take_map(fn(token) {
        case token {
          token.Name(content) -> Some(Index(content, 0))
          _ -> None
        }
      }),
    )
    use _ <- do(chomp.token(token.Colon))
    return(name)
  })
}

fn parse_type_parameter() {
  use _ <- do(chomp.token(token.LeftParen))
  use content <- do(parse_kind() |> chomp.sequence(chomp.token(token.Comma)))
  use _ <- do(chomp.token(token.RightParen))
  return(content)
}

fn parse_return() {
  use _ <- do(chomp.token(token.RightArrow))
  use content <- do(parse_kind())
  return(content)
}

fn parse_tuple() {
  use _ <- do(chomp.token(token.Hash))
  use _ <- do(chomp.token(token.LeftParen))
  use content <- do(parse_kind() |> chomp.sequence(chomp.token(token.Comma)))
  use _ <- do(chomp.token(token.RightParen))
  return(Tuple(content))
}

fn parse_fn() {
  use _ <- do(chomp.token(token.Fn))
  use _ <- do(chomp.optional(parse_name()))
  use _ <- do(chomp.optional(chomp.token(token.LeftParen)))
  use content <- do(
    {
      use _ <- do(chomp.optional(parse_label()))
      parse_kind()
    }
    |> chomp.sequence(chomp.token(token.Comma)),
  )
  use _ <- do(chomp.optional(chomp.token(token.RightParen)))
  use content_ <- do(parse_return())
  return(Function(content, content_))
}

fn parse_kind() {
  chomp.one_of([
    parse_fn(),
    parse_tuple(),
    chomp.backtrackable(parse_qualified_name()),
    parse_name(),
  ])
}

pub fn parse_function(input: String) {
  let tokens =
    input
    |> glexer.new
    |> glexer.lex
    |> list.map(fn(elem) {
      lexer.Token(span.Span({ elem.1 }.byte_offset, 0, 0, 0), "", elem.0)
    })
  parse_fn()
  |> chomp.run(tokens, _)
  |> result.map(replace_indexed(#(dict.new(), 0), _))
  |> result.map(pair.first)
}

fn replace_indexed(
  indexes: #(Dict(String, Int), Int),
  kind: Kind,
) -> #(Kind, #(Dict(String, Int), Int)) {
  let #(indexes, current) = indexes
  case kind {
    Index(name, _) -> {
      case dict.get(indexes, name) {
        Ok(value) -> #(Index(name, value), #(indexes, current))
        Error(_) -> #(Index(name, current), #(
          dict.insert(indexes, name, current),
          current + 1,
        ))
      }
    }
    Custom(name, kinds) -> {
      let #(new_kinds, accs) =
        list.fold(kinds, #([], #(indexes, current)), fn(acc, val) {
          let res = replace_indexed(acc.1, val)
          #([res.0, ..acc.0], res.1)
        })
      #(Custom(name, list.reverse(new_kinds)), accs)
    }
    Function(kinds, return_value) -> {
      let #(new_kinds, accs) =
        list.fold(kinds, #([], #(indexes, current)), fn(acc, val) {
          let res = replace_indexed(acc.1, val)
          #([res.0, ..acc.0], res.1)
        })
      let #(return_value, accs) = replace_indexed(accs, return_value)
      #(Function(list.reverse(new_kinds), return_value), accs)
    }
    Tuple(kinds) -> {
      let #(new_kinds, accs) =
        list.fold(kinds, #([], #(indexes, current)), fn(acc, val) {
          let res = replace_indexed(acc.1, val)
          #([res.0, ..acc.0], res.1)
        })
      #(Tuple(list.reverse(new_kinds)), accs)
    }
  }
}
