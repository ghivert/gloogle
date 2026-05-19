import cell
import gleam/bool
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/otp/supervision
import gleam/result
import gleam/unsafe
import jupiter/context.{type Context}
import jupiter/gleam/parse
import jupiter/gleam/type_search/search
import pog

const name = "jupiter_type_search"

pub type Msg {
  Add(signature: String, id: String)
}

pub fn add(signature signature: String, id id: String) {
  let subject = subject()
  let msg = Add(signature:, id:)
  process.send(subject, msg)
}

pub fn find(ctx: Context, signature) {
  signature
  |> parse.parse_function
  |> result.replace_error(Nil)
  |> result.try(permutation_search(ctx, _))
  |> option.from_result
}

pub fn worker(ctx: Context) {
  use <- supervision.worker()
  let name = atom.create(name) |> unsafe.coerce
  actor.new_with_initialiser(120_000, fn(subject) {
    let #(_, search) = initialise_type_search(ctx.db)
    let assert Ok(_) = cell.write(ctx.search, search)
    actor.initialised(ctx)
    |> actor.returning(subject)
    |> Ok
  })
  |> actor.named(name)
  |> actor.on_message(loop)
  |> actor.start
}

fn initialise_type_search(db: pog.Connection) {
  use search, #(sig, id) <- compute_rows(0, db, #(0, search.empty()))
  let #(idx, searches) = search
  sig
  |> parse.parse_function
  |> result.map(fn(kind) { #(idx + 1, search.add(searches, kind, id)) })
  |> result.unwrap(search)
}

fn loop(ctx: Context, msg: Msg) -> actor.Next(Context, Msg) {
  let assert Ok(search) = cell.read(ctx.search)
  let assert Ok(_) =
    msg.signature
    |> parse.parse_function
    |> result.map(search.add(search, _, msg.id))
    |> result.unwrap(search)
    |> cell.write(ctx.search, _)
  actor.continue(ctx)
}

fn compute_rows(
  offset: Int,
  db: pog.Connection,
  default: a,
  next: fn(a, #(String, String)) -> a,
) -> a {
  let rows =
    "SELECT signature_, id
     FROM package_type_fun_signature
     WHERE kind = 'function'
     ORDER BY id ASC
     LIMIT 1000
     OFFSET $1"
    |> pog.query
    |> pog.parameter(pog.int(offset))
    |> pog.returning({
      use signature <- decode.field("signature_", decode.string)
      use id <- decode.field("id", decode.string)
      decode.success(#(signature, id))
    })
    |> pog.execute(db)
    |> result.map(fn(r) { r.rows })
    |> result.unwrap([])
  use <- bool.guard(when: list.is_empty(rows), return: default)
  list.fold(rows, default, next)
  |> compute_rows(offset + 1000, db, _, next)
}

fn subject() -> process.Subject(Msg) {
  atom.create(name)
  |> unsafe.coerce
  |> process.named_subject
}

fn is_permutable(list: List(a), len: Int) {
  use <- bool.guard(when: len > 4, return: False)
  case list {
    [_, ..rest] -> is_permutable(rest, len + 1)
    [] -> True
  }
}

fn permutation_search(
  ctx: Context,
  kind: parse.Kind,
) -> Result(List(String), Nil) {
  use search <- result.try(cell.read(ctx.search))
  case kind {
    parse.DiscardName -> Error(Nil)
    parse.Index(_, _) -> Error(Nil)
    parse.Custom(_, _) -> Error(Nil)
    parse.Tuple(_) -> Error(Nil)
    parse.Function(params, return) -> {
      case is_permutable(params, 0) {
        False -> search.find(search, kind, ctx.db)
        True -> {
          Ok({
            let permutations = list.permutations(params)
            use permutation <- list.flat_map(permutations)
            parse.Function(permutation, return)
            |> search.find(search, _, ctx.db)
            |> result.unwrap([])
          })
        }
      }
    }
  }
}
