import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/list_
import gleam/option.{type Option, None}
import gleam/pair
import gleam/result
import jupiter/gleam/parse.{type Kind, Function}
import jupiter/postgres/queries
import pog

pub type TypeSearch {
  TypeSearch(keys: Keys, rows: List(String))
}

pub type Keys {
  Keys(keys: Dict(String, Keys), next: Option(TypeSearch))
}

pub fn empty() {
  let keys = Keys(dict.new(), None)
  TypeSearch(keys: keys, rows: [])
}

fn update_keys(
  keys: Keys,
  kinds: List(Kind),
  updater: fn(TypeSearch) -> TypeSearch,
) -> Keys {
  case kinds {
    [] -> update_empty_keys(keys, updater)
    [kind, ..rest] -> {
      Keys(..keys, keys: {
        case kind {
          parse.DiscardName -> panic as "No Discard name in add"
          parse.Index(_value, index) -> {
            let value = int.to_string(index)
            use keys <- dict.upsert(keys.keys, value)
            let keys = option.unwrap(keys, Keys(keys: dict.new(), next: None))
            update_keys(keys, rest, updater)
          }
          parse.Custom(value, kinds) -> {
            use keys <- dict.upsert(keys.keys, value)
            let keys = option.unwrap(keys, Keys(keys: dict.new(), next: None))
            update_keys(keys, list.append(kinds, rest), updater)
          }
          parse.Function(kinds, return) -> {
            let kinds = list_.postpend(kinds, return)
            use keys <- dict.upsert(keys.keys, "fn")
            let keys = option.unwrap(keys, Keys(keys: dict.new(), next: None))
            update_keys(keys, list.append(kinds, rest), updater)
          }
          parse.Tuple(kinds) -> {
            use keys <- dict.upsert(keys.keys, "#()")
            let keys = option.unwrap(keys, Keys(keys: dict.new(), next: None))
            update_keys(keys, list.append(kinds, rest), updater)
          }
        }
      })
    }
  }
}

fn update_empty_keys(keys: Keys, updater: fn(TypeSearch) -> TypeSearch) -> Keys {
  Keys(..keys, next: {
    keys.next
    |> option.lazy_unwrap(empty)
    |> updater
    |> option.Some
  })
}

fn do_add(searches: TypeSearch, kinds: List(Kind), id: String) -> TypeSearch {
  case kinds {
    [] -> TypeSearch(..searches, rows: [id, ..searches.rows])
    [kind, ..rest] -> {
      TypeSearch(..searches, keys: {
        use keys <- update_keys(searches.keys, [kind])
        do_add(keys, rest, id)
      })
    }
  }
}

pub fn add(searches: TypeSearch, kind: Kind, id: String) {
  case kind {
    parse.DiscardName -> searches
    parse.Index(_, _) -> searches
    parse.Custom(_, _) -> searches
    parse.Tuple(_) -> searches
    Function(kinds, return_value) -> {
      kinds
      |> list_.postpend(return_value)
      |> do_add(searches, _, id)
    }
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
  db: pog.Connection,
) -> List(#(Keys, Dict(Int, String))) {
  case kind {
    parse.DiscardName -> {
      dict.values(keys.keys)
      |> extract_all_keys
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
      dict.get(keys.keys, value)
      |> result.map(get_kinds_next_tree(_, env, params, db))
      |> result.unwrap([])
    }
    parse.Function(params, return) -> {
      let params = list_.postpend(params, return)
      dict.get(keys.keys, "fn")
      |> result.map(get_kinds_next_tree(_, env, params, db))
      |> result.unwrap([])
    }
    parse.Tuple(params) -> {
      dict.get(keys.keys, "#()")
      |> result.map(get_kinds_next_tree(_, env, params, db))
      |> result.unwrap([])
    }
  }
}

fn get_kinds_next_tree(
  keys: Keys,
  env: Dict(Int, String),
  params: List(Kind),
  db: pog.Connection,
) -> List(#(Keys, Dict(Int, String))) {
  use envs, param <- list.fold(params, [#(keys, env)])
  use #(key, env) <- list.flat_map(envs)
  get_next_tree(key, param, env, db)
}

fn find_next_tree(
  keys: Keys,
  kind: Kind,
  kinds: List(Kind),
  env: Dict(Int, String),
  db: pog.Connection,
) -> List(String) {
  case kind {
    parse.DiscardName -> {
      let values = get_next_tree(keys, kind, env, db)
      use #(keys, env) <- list.flat_map(values)
      keys.next
      |> option.map(do_find(_, kinds, env, db))
      |> option.unwrap([])
    }
    parse.Index(_value, _index) -> {
      let values = get_next_tree(keys, kind, env, db)
      use #(keys, env) <- list.flat_map(values)
      keys.next
      |> option.map(do_find(_, kinds, env, db))
      |> option.unwrap([])
    }
    parse.Custom(value, params) -> {
      let values = result.unwrap(queries.find_similar_type_names(db, value), [])
      use value <- list.flat_map(values)
      case dict.get(keys.keys, value) {
        Error(_) -> []
        Ok(keys) -> {
          let trees = get_kinds_next_tree(keys, env, params, db)
          use #(key, env) <- list.flat_map(trees)
          key.next
          |> option.map(do_find(_, kinds, env, db))
          |> option.unwrap([])
        }
      }
    }
    parse.Function(params, return) -> {
      case dict.get(keys.keys, "fn") {
        Error(_) -> []
        Ok(keys) -> {
          let params = list_.postpend(params, return)
          let trees = get_kinds_next_tree(keys, env, params, db)
          use #(key, env) <- list.flat_map(trees)
          key.next
          |> option.map(do_find(_, kinds, env, db))
          |> option.unwrap([])
        }
      }
    }
    parse.Tuple(params) -> {
      case dict.get(keys.keys, "#()") {
        Error(_) -> []
        Ok(keys) -> {
          let trees = get_kinds_next_tree(keys, env, params, db)
          use #(key, env) <- list.flat_map(trees)
          key.next
          |> option.map(do_find(_, kinds, env, db))
          |> option.unwrap([])
        }
      }
    }
  }
}

fn do_find(
  searches: TypeSearch,
  kinds: List(Kind),
  env: Dict(Int, String),
  db: pog.Connection,
) -> List(String) {
  case kinds {
    [kind, ..rest] -> find_next_tree(searches.keys, kind, rest, env, db)
    [] -> searches.rows
  }
}

pub fn find(searches: TypeSearch, kind: Kind, db: pog.Connection) {
  case kind {
    parse.DiscardName -> Error(Nil)
    parse.Index(_, _) -> Error(Nil)
    parse.Custom(_, _) -> Error(Nil)
    parse.Tuple(_) -> Error(Nil)
    Function(kinds, return_value) ->
      kinds
      |> list_.postpend(return_value)
      |> do_find(searches, _, dict.new(), db)
      |> Ok
  }
}
