import backend/gleam/parse
import backend/gleam/type_search.{type TypeSearch}
import backend/gleam/type_search/msg
import gleam/bool
import gleam/dynamic
import gleam/erlang/process
import gleam/function
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/pgo
import gleam/result

pub type State {
  State(db: pgo.Connection, search: TypeSearch)
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
  actor.start_spec(actor.Spec(init, init_timeout: 120_000, loop: loop))
}

fn loop(msg: msg.Msg, state: State) -> actor.Next(msg.Msg, State) {
  case msg {
    msg.Find(subject, signature) -> {
      signature
      |> parse.parse_function
      |> result.nil_error
      |> result.then(permutation_search(state, _))
      |> option.from_result
      |> function.tap(fn(res) { process.send(subject, res) })
      actor.continue(state)
    }
    msg.Add(signature, id) -> {
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
        type_search.find(state.search, kind, state.db)
      })
      Ok({
        let permutations = list.permutations(params)
        use permutation <- list.flat_map(permutations)
        parse.Function(permutation, return)
        |> type_search.find(state.search, _, state.db)
        |> result.unwrap([])
      })
    }
    _ -> Error(Nil)
  }
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
