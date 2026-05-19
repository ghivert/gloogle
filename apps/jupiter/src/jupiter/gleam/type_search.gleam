import gleam/bool
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision
import gleam/result
import gleam/unsafe
import jupiter/gleam/parse
import jupiter/gleam/type_search/search.{type TypeSearch}
import pog

const name = "jupiter_type_search"

pub type State {
  State(db: pog.Connection, search: TypeSearch)
}

pub type Msg {
  Add(signature: String, id: Int)
}

pub fn subject() -> process.Subject(Msg) {
  atom.create(name)
  |> unsafe.coerce
  |> process.named_subject
}

pub fn add(signature signature: String, id id: Int) {
  let subject = subject()
  let msg = Add(signature:, id:)
  process.send(subject, msg)
}

pub fn worker(db: pog.Connection) {
  use <- supervision.worker()
  let name = atom.create(name) |> unsafe.coerce
  actor.new_with_initialiser(120_000, fn(subject) {
    let #(_, search) = initialise_type_search(db)
    actor.initialised(State(db, search))
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

fn loop(state: State, msg: Msg) -> actor.Next(State, Msg) {
  // msg.Find(subject, signature) -> {
  //   signature
  //   |> parse.parse_function
  //   |> result.replace_error(Nil)
  //   |> result.try(permutation_search(state, _))
  //   |> option.from_result
  //   |> function_.tap(fn(res) { process.send(subject, res) })
  //   actor.continue(state)
  // }
  actor.continue({
    State(..state, search: {
      msg.signature
      |> parse.parse_function
      |> result.map(search.add(state.search, _, msg.id))
      |> result.unwrap(state.search)
    })
  })
}

fn is_permutable(list: List(a), len: Int) {
  case list {
    _ if len > 4 -> False
    [_, ..rest] -> is_permutable(rest, len + 1)
    [] -> True
  }
}

fn permutation_search(state: State, kind: parse.Kind) {
  case kind {
    parse.Function(params, return) -> {
      let permutable = is_permutable(params, 0)
      use <- bool.lazy_guard(when: !permutable, return: fn() {
        search.find(state.search, kind, state.db)
      })
      Ok({
        let permutations = list.permutations(params)
        use permutation <- list.flat_map(permutations)
        parse.Function(permutation, return)
        |> search.find(state.search, _, state.db)
        |> result.unwrap([])
      })
    }
    _ -> Error(Nil)
  }
}

fn compute_rows(
  offset: Int,
  db: pog.Connection,
  default: a,
  next: fn(a, #(String, Int)) -> a,
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
      use id <- decode.field("id", decode.int)
      decode.success(#(signature, id))
    })
    |> pog.execute(db)
    |> result.map(fn(r) { r.rows })
    |> result.unwrap([])
  use <- bool.guard(when: list.is_empty(rows), return: default)
  list.fold(rows, default, next)
  |> compute_rows(offset + 1000, db, _, next)
}
