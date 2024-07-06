import backend/gleam/parse
import backend/gleam/type_search.{type TypeSearch}
import gleam/bool
import gleam/dynamic
import gleam/erlang/process.{type Subject}
import gleam/function
import gleam/list
import gleam/option.{type Option}
import gleam/otp/actor
import gleam/pgo
import gleam/result

pub type State {
  State(db: pgo.Connection, search: TypeSearch)
}

pub type Msg {
  Find(Subject(Option(List(Int))), String)
  Add(String, Int)
}

pub fn init(db: pgo.Connection) {
  let init = fn() {
    let search =
      compute_rows(0, db, #(0, type_search.empty()), {
        fn(search: #(Int, TypeSearch), row: #(String, Int)) {
          let #(signature, id) = row
          signature
          |> parse.parse_function
          |> result.map(fn(kind) {
            #(search.0 + 1, type_search.add(search.1, kind, id))
          })
          |> result.unwrap(search)
        }
      })
    process.new_selector()
    |> process.selecting(process.new_subject(), function.identity)
    |> actor.Ready(State(db, search.1), _)
  }
  actor.start_spec(actor.Spec(init, init_timeout: 10_000, loop: loop))
}

fn loop(msg: Msg, state: State) -> actor.Next(Msg, State) {
  case msg {
    Find(subject, signature) -> {
      signature
      |> parse.parse_function
      |> result.map(type_search.find(state.search, _))
      |> result.unwrap(option.None)
      |> function.tap(fn(res) { process.send(subject, res) })
      actor.continue(state)
    }
    Add(signature, id) -> {
      signature
      |> parse.parse_function
      |> result.map(fn(kind) { type_search.add(state.search, kind, id) })
      |> result.unwrap(state.search)
      |> fn(s) { State(..state, search: s) }
      |> actor.continue
    }
  }
  actor.continue(state)
}

fn compute_rows(
  offset: Int,
  db: pgo.Connection,
  default: a,
  next: fn(a, #(String, Int)) -> a,
) {
  let decoder = dynamic.tuple2(dynamic.string, dynamic.int)
  let rows =
    "SELECT signature_, id
     FROM package_type_fun_signature
     WHERE kind = 'function'
     ORDER BY id ASC
     LIMIT 1000
     OFFSET $1"
    |> pgo.execute(db, [pgo.int(offset)], decoder)
    |> result.map(fn(r) { r.rows })
    |> result.unwrap([])
  use <- bool.guard(when: list.is_empty(rows), return: default)
  list.fold(rows, default, next)
  |> compute_rows(offset + 1000, db, _, next)
}
