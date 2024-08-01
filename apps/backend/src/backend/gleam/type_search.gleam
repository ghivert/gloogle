import backend/gleam/parse.{type Kind, Function}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/pair
import gleam/result
import gleam/string

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

fn find_next_tree(
  keys: Keys,
  kind: Kind,
  association_params: Dict(Int, String),
) -> #(List(Keys), Dict(Int, String)) {
  case kind {
    parse.DiscardName -> {
      #(dict.values(keys.keys), association_params)
    }
    parse.Index(_value, index) -> {
      case dict.get(association_params, index) {
        Ok(content) -> {
          dict.get(keys.keys, content)
          |> result.map(list.wrap)
          |> result.unwrap([])
          |> pair.new(association_params)
        }
        Error(_) -> {
          let new_key =
            dict.keys(keys.keys)
            |> list.filter(fn(a) {
              a != "fn"
              && a != "#()"
              && case string.first(a) {
                Ok(a) -> string.uppercase(a) == a
                Error(_) -> False
              }
            })
            |> list.sort(string.compare)
            |> list.find(fn(a) {
              !list.contains(dict.values(association_params), a)
            })
          case new_key {
            Error(_) -> #([], association_params)
            Ok(s) ->
              find_next_tree(
                keys,
                kind,
                dict.insert(association_params, index, s),
              )
          }
        }
      }
    }
    parse.Custom(value, params) ->
      case dict.get(keys.keys, value) {
        Error(_) -> #([], association_params)
        Ok(keys) -> {
          use #(keys, assoc), val <- list.fold(params, #(
            [keys],
            association_params,
          ))
          list.fold(keys, #([], assoc), fn(a, key) {
            find_next_tree(key, val, a.1)
            |> pair.map_first(list.append(a.0, _))
          })
        }
      }
    parse.Function(kinds, return) -> {
      let kinds = postpend(kinds, return)
      case dict.get(keys.keys, "fn") {
        Error(_) -> #([], association_params)
        Ok(keys) -> {
          use #(keys, assoc), val <- list.fold(kinds, #(
            [keys],
            association_params,
          ))
          list.fold(keys, #([], assoc), fn(a, key) {
            find_next_tree(key, val, a.1)
            |> pair.map_first(list.append(a.0, _))
          })
        }
      }
    }
    parse.Tuple(kinds) -> {
      case dict.get(keys.keys, "#()") {
        Error(_) -> #([], association_params)
        Ok(keys) -> {
          use #(keys, assoc), val <- list.fold(kinds, #(
            [keys],
            association_params,
          ))
          list.fold(keys, #([], assoc), fn(a, key) {
            find_next_tree(key, val, a.1)
            |> pair.map_first(list.append(a.0, _))
          })
        }
      }
    }
  }
}

fn do_find(
  searches: TypeSearch,
  kinds: List(Kind),
  association_params: Dict(Int, String),
) {
  case kinds {
    [] -> searches.rows
    [kind, ..rest] -> {
      let #(keys, assoc) =
        find_next_tree(searches.keys, kind, association_params)
      keys
      |> list.flat_map(fn(k) {
        k.next |> option.map(list.wrap) |> option.unwrap([])
      })
      |> list.flat_map(do_find(_, rest, assoc))
    }
  }
}

pub fn find(searches: TypeSearch, kind: Kind) {
  case kind {
    Function(kinds, return_value) ->
      kinds
      |> postpend(return_value)
      |> do_find(searches, _, dict.new())
      |> Ok
    _ -> Error(Nil)
  }
}
