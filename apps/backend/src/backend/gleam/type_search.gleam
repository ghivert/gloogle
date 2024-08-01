import backend/gleam/parse.{type Kind, Function}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/result

pub type TypeSearch {
  TypeSearch(keys: Keys, rows: List(Int))
}

pub type Keys {
  Keys(keys: Dict(String, Keys), next: Option(TypeSearch))
}

pub fn empty() {
  let keys = Keys(dict.new(), option.None)
  TypeSearch(keys: keys, rows: [])
}

fn postpend(list: List(a), value: a) {
  list
  |> list.reverse
  |> list.prepend(value)
  |> list.reverse
}

fn update_keys(
  keys: Keys,
  kinds: List(Kind),
  updater: fn(TypeSearch) -> TypeSearch,
) -> Keys {
  case kinds {
    [] -> {
      let next =
        keys.next
        |> option.unwrap(empty())
        |> updater
        |> option.Some
      Keys(..keys, next: next)
    }
    [k, ..rest] -> {
      let next = option.None
      let new_keys = case k {
        parse.DiscardName -> panic as "No Discard name in add"
        parse.Index(_value, index) -> {
          let value = int.to_string(index)
          dict.update(keys.keys, value, fn(k) {
            let k = option.unwrap(k, Keys(keys: dict.new(), next: next))
            update_keys(k, rest, updater)
          })
        }
        parse.Custom(value, kinds) ->
          dict.update(keys.keys, value, fn(k) {
            let k = option.unwrap(k, Keys(keys: dict.new(), next: next))
            update_keys(k, list.append(kinds, rest), updater)
          })
        parse.Function(kinds, return) -> {
          let kinds = postpend(kinds, return)
          dict.update(keys.keys, "fn", fn(k) {
            let k = option.unwrap(k, Keys(keys: dict.new(), next: next))
            update_keys(k, list.append(kinds, rest), updater)
          })
        }
        parse.Tuple(kinds) -> {
          dict.update(keys.keys, "#()", fn(k) {
            let k = option.unwrap(k, Keys(keys: dict.new(), next: next))
            update_keys(k, list.append(kinds, rest), updater)
          })
        }
      }
      Keys(..keys, keys: new_keys)
    }
  }
}

fn do_add(searches: TypeSearch, kinds: List(Kind), id: Int) -> TypeSearch {
  case kinds {
    [] -> TypeSearch(..searches, rows: [id, ..searches.rows])
    [kind, ..rest] -> {
      TypeSearch(
        ..searches,
        keys: update_keys(searches.keys, [kind], do_add(_, rest, id)),
      )
    }
  }
}

pub fn add(searches: TypeSearch, kind: Kind, id: Int) {
  case kind {
    Function(kinds, return_value) -> {
      let kinds = postpend(kinds, return_value)
      do_add(searches, kinds, id)
    }
    _ -> searches
  }
}

fn find_next_tree(keys: Keys, kind: Kind) -> List(Keys) {
  case kind {
    parse.DiscardName -> dict.values(keys.keys)
    parse.Index(_value, index) ->
      dict.get(keys.keys, int.to_string(index))
      |> result.map(list.wrap)
      |> result.unwrap([])
    parse.Custom(value, params) ->
      case dict.get(keys.keys, value) {
        Error(_) -> []
        Ok(keys) -> {
          use acc, val <- list.fold(params, [keys])
          list.flat_map(acc, find_next_tree(_, val))
        }
      }
    parse.Function(kinds, return) -> {
      let kinds = postpend(kinds, return)
      case dict.get(keys.keys, "fn") {
        Error(_) -> []
        Ok(keys) -> {
          use acc, val <- list.fold(kinds, [keys])
          list.flat_map(acc, find_next_tree(_, val))
        }
      }
    }
    parse.Tuple(kinds) -> {
      case dict.get(keys.keys, "#()") {
        Error(_) -> []
        Ok(keys) -> {
          use acc, val <- list.fold(kinds, [keys])
          list.flat_map(acc, find_next_tree(_, val))
        }
      }
    }
  }
}

fn do_find(searches: TypeSearch, kinds: List(Kind)) {
  case kinds {
    [] -> searches.rows
    [kind, ..rest] ->
      find_next_tree(searches.keys, kind)
      |> list.flat_map(fn(k) {
        k.next |> option.map(list.wrap) |> option.unwrap([])
      })
      |> list.flat_map(do_find(_, rest))
  }
}

pub fn find(searches: TypeSearch, kind: Kind) {
  case kind {
    Function(kinds, return_value) ->
      kinds
      |> postpend(return_value)
      |> do_find(searches, _)
      |> Ok
    _ -> Error(Nil)
  }
}
