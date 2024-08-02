import backend/gleam/parse.{type Kind, Function}
import backend/postgres/queries
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/pair
import gleam/pgo
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

/// Extract all keys. Because we can have Int, Option(a) or Result(a, b), we
/// have to extract all intermediate next nodes, because _ can be anything.
fn extract_all_keys(keys: List(Keys)) -> List(Keys) {
  use key <- list.flat_map(keys)
  [key, ..extract_all_keys(dict.values(key.keys))]
}

/// Get the underlying ending Keys for a Kind, associated with its local
/// environment for free variables.
fn get_next_tree(
  keys: Keys,
  kind: Kind,
  env: Dict(Int, String),
  db: pgo.Connection,
) -> List(#(Keys, Dict(Int, String))) {
  case kind {
    parse.DiscardName -> {
      extract_all_keys(dict.values(keys.keys))
      |> list.map(pair.new(_, env))
    }
    parse.Index(_value, index) -> {
      case dict.get(env, index) {
        Ok(content) -> {
          dict.get(keys.keys, content)
          |> result.map(pair.new(_, env))
          |> result.map(list.wrap)
          |> result.unwrap([])
        }
        Error(_) -> {
          let existing_values = dict.values(env)
          dict.keys(keys.keys)
          |> list.filter(fn(a) { int.parse(a) |> result.is_ok })
          |> list.filter(fn(a) { !list.contains(existing_values, a) })
          |> list.flat_map(fn(a) {
            get_next_tree(keys, kind, dict.insert(env, index, a), db)
          })
        }
      }
    }
    parse.Custom(value, params) -> {
      let values = result.unwrap(queries.find_similar_type_names(db, value), [])
      use value <- list.flat_map(values)
      case dict.get(keys.keys, value) {
        Error(_) -> []
        Ok(keys) -> {
          use envs, kind <- list.fold(params, [#(keys, env)])
          use env <- list.flat_map(envs)
          let #(key, env) = env
          get_next_tree(key, kind, env, db)
        }
      }
    }
    parse.Function(params, return) -> {
      let params = postpend(params, return)
      case dict.get(keys.keys, "fn") {
        Error(_) -> []
        Ok(keys) -> {
          use envs, param <- list.fold(params, [#(keys, env)])
          use env <- list.flat_map(envs)
          let #(key, env) = env
          get_next_tree(key, param, env, db)
        }
      }
    }
    parse.Tuple(params) -> {
      case dict.get(keys.keys, "#()") {
        Error(_) -> []
        Ok(keys) -> {
          use envs, param <- list.fold(params, [#(keys, env)])
          use env <- list.flat_map(envs)
          let #(key, env) = env
          get_next_tree(key, param, env, db)
        }
      }
    }
  }
}

fn find_next_tree(
  keys: Keys,
  kind: Kind,
  kinds: List(Kind),
  env: Dict(Int, String),
  db: pgo.Connection,
) -> List(Int) {
  case kind {
    parse.DiscardName -> {
      let values = get_next_tree(keys, kind, env, db)
      use #(keys, env) <- list.flat_map(values)
      option.map(keys.next, do_find(_, kinds, env, db))
      |> option.unwrap([])
    }
    parse.Index(_value, _index) -> {
      let values = get_next_tree(keys, kind, env, db)
      use #(keys, env) <- list.flat_map(values)
      option.map(keys.next, do_find(_, kinds, env, db))
      |> option.unwrap([])
    }
    parse.Custom(value, params) -> {
      let values = result.unwrap(queries.find_similar_type_names(db, value), [])
      use value <- list.flat_map(values)
      case dict.get(keys.keys, value) {
        Error(_) -> []
        Ok(keys) -> {
          list.fold(params, [#(keys, env)], fn(acc, param) {
            list.flat_map(acc, fn(a) { get_next_tree(a.0, param, a.1, db) })
          })
          |> list.flat_map(fn(val) {
            let #(key, env) = val
            option.map(key.next, do_find(_, kinds, env, db))
            |> option.unwrap([])
          })
        }
      }
    }
    parse.Function(params, return) -> {
      let params = postpend(params, return)
      case dict.get(keys.keys, "fn") {
        Error(_) -> []
        Ok(keys) -> {
          list.fold(params, [#(keys, env)], fn(acc, param) {
            list.flat_map(acc, fn(a) { get_next_tree(a.0, param, a.1, db) })
          })
          |> list.flat_map(fn(val) {
            let #(key, env) = val
            option.map(key.next, do_find(_, kinds, env, db))
            |> option.unwrap([])
          })
        }
      }
    }
    parse.Tuple(params) -> {
      case dict.get(keys.keys, "#()") {
        Error(_) -> []
        Ok(keys) -> {
          list.fold(params, [#(keys, env)], fn(acc, param) {
            list.flat_map(acc, fn(a) { get_next_tree(a.0, param, a.1, db) })
          })
          |> list.flat_map(fn(val) {
            let #(key, env) = val
            option.map(key.next, do_find(_, kinds, env, db))
            |> option.unwrap([])
          })
        }
      }
    }
  }
}

fn do_find(
  searches: TypeSearch,
  kinds: List(Kind),
  env: Dict(Int, String),
  db: pgo.Connection,
) {
  case kinds {
    [] -> searches.rows
    [kind, ..rest] -> find_next_tree(searches.keys, kind, rest, env, db)
  }
}

pub fn find(searches: TypeSearch, kind: Kind, db: pgo.Connection) {
  case kind {
    Function(kinds, return_value) ->
      kinds
      |> postpend(return_value)
      |> do_find(searches, _, dict.new(), db)
      |> Ok
    _ -> Error(Nil)
  }
}
