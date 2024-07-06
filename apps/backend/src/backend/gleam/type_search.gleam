import backend/gleam/parse.{type Kind, Function}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option
import gleam/result

pub type TypeSearch {
  TypeSearch(entries: Dict(Kind, TypeSearch), rows: List(Int))
}

pub fn empty() {
  TypeSearch(dict.new(), [])
}

fn postpend(list: List(a), value: a) {
  list
  |> list.reverse
  |> list.prepend(value)
  |> list.reverse
}

fn add_index(list: List(a)) {
  use elem <- list.map(list)
  #(elem, option.None)
}

fn do_add(searches: TypeSearch, kinds: List(#(Kind, option.Option(Int)))) {
  case kinds {
    [] -> searches
    [#(kind, option.Some(id))] ->
      dict.get(searches.entries, kind)
      |> result.unwrap(empty())
      |> fn(s: TypeSearch) {
        let rows = case list.contains(s.rows, id) {
          True -> s.rows
          False -> [id, ..s.rows]
        }
        TypeSearch(..s, rows: rows)
      }
      |> dict.insert(searches.entries, kind, _)
      |> fn(a) { TypeSearch(..searches, entries: a) }
    [#(kind, _), ..rest] ->
      dict.get(searches.entries, kind)
      |> result.unwrap(empty())
      |> do_add(rest)
      |> dict.insert(searches.entries, kind, _)
      |> fn(s) { TypeSearch(..searches, entries: s) }
  }
}

pub fn add(searches: TypeSearch, kind: Kind, id: Int) {
  case kind {
    Function(kinds, return_value) ->
      kinds
      |> add_index
      |> postpend(#(return_value, option.Some(id)))
      |> do_add(searches, _)
    _ -> searches
  }
}

fn do_find(searches: TypeSearch, kinds: List(Kind)) {
  case kinds {
    [] -> Error(Nil)
    [kind] -> dict.get(searches.entries, kind) |> result.map(fn(s) { s.rows })
    [kind, ..rest] ->
      dict.get(searches.entries, kind) |> result.then(do_find(_, rest))
  }
}

pub fn find(searches: TypeSearch, kind: Kind) {
  case kind {
    Function(kinds, return_value) ->
      kinds
      |> postpend(return_value)
      |> do_find(searches, _)
      |> option.from_result
    _ -> option.None
  }
}
