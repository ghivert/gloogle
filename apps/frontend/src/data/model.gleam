import data/kind
import data/msg.{type Msg}
import data/package.{type Package}
import data/search_result.{type SearchResult, type SearchResults}
import frontend/router
import frontend/view/body/cache
import gleam/dict.{type Dict}
import gleam/function
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/pair
import gleam/result
import gleam/string
import lustre/element.{type Element}

pub type Index =
  List(#(#(String, String), List(#(String, String))))

pub type Model {
  Model(
    input: String,
    search_results: Dict(String, SearchResults),
    index: Index,
    loading: Bool,
    view_cache: Dict(String, Element(Msg)),
    route: router.Route,
    trendings: Option(List(Package)),
    submitted_input: String,
    keep_functions: Bool,
    keep_types: Bool,
    keep_aliases: Bool,
    keep_documented: Bool,
    show_old_packages: Bool,
    show_documentation_search: Bool,
    show_vector_search: Bool,
  )
}

pub fn init() {
  let search_results = search_result.Start
  let index = compute_index(search_results)
  Model(
    input: "",
    search_results: dict.new(),
    index: index,
    loading: False,
    view_cache: dict.new(),
    route: router.Home,
    trendings: option.None,
    submitted_input: "",
    keep_functions: False,
    keep_types: False,
    keep_aliases: False,
    keep_documented: False,
    show_old_packages: False,
    show_documentation_search: False,
    show_vector_search: False,
  )
}

pub fn update_route(model: Model, route: router.Route) {
  Model(..model, route: route)
}

pub fn update_submitted_input(model: Model) {
  Model(..model, submitted_input: model.input)
}

pub fn update_trendings(model: Model, trendings: List(Package)) {
  model.trendings
  |> option.unwrap([])
  |> list.append(trendings)
  |> option.Some
  |> fn(t) { Model(..model, trendings: t) }
}

pub fn toggle_loading(model: Model) {
  Model(..model, loading: !model.loading)
}

pub fn update_input(model: Model, content: String) {
  Model(..model, input: content)
}

pub fn search_key(key key: String, model model: Model) {
  key
  <> string.inspect([
    model.keep_functions,
    model.keep_types,
    model.keep_aliases,
    model.keep_documented,
    model.show_old_packages,
    model.show_documentation_search,
    model.show_vector_search,
  ])
}

fn default_search_key(key key: String) {
  key <> string.inspect([False, False, False, False, True, True, True])
}

pub fn update_search_results(
  model: Model,
  key: String,
  search_results: SearchResults,
) {
  let key = default_search_key(key: key)
  let index = compute_index(search_results)
  let view_cache = case search_results {
    search_result.Start | search_result.InternalServerError -> model.view_cache
    search_result.SearchResults(types, e, m, s, d, mods) ->
      cache.cache_search_results(
        model.submitted_input,
        index,
        types,
        e,
        m,
        s,
        d,
        mods,
      )
      |> dict.insert(model.view_cache, key, _)
  }
  Model(
    ..model,
    search_results: dict.insert(model.search_results, key, search_results),
    index: index,
    view_cache: view_cache,
  )
  |> update_search_results_filter
}

fn is_higher(new: List(Int), old: List(Int)) {
  case list.first(new), list.first(old) {
    Error(_), Error(_) -> True
    Ok(_), Error(_) -> True
    Error(_), Ok(_) -> False
    Ok(part1), Ok(part2) ->
      case part1 == part2 {
        False -> part1 > part2
        True ->
          is_higher(
            list.rest(new) |> result.unwrap([]),
            list.rest(old) |> result.unwrap([]),
          )
      }
  }
}

fn extract_package_version(
  acc: Dict(String, String),
  search_result: search_result.SearchResult,
) -> Dict(String, String) {
  case dict.get(acc, search_result.package_name) {
    Error(_) ->
      dict.insert(acc, search_result.package_name, search_result.version)
    Ok(content) -> {
      let old =
        string.split(content, ".")
        |> list.map(int.parse)
        |> list.map(result.unwrap(_, 0))
      let new =
        string.split(search_result.version, ".")
        |> list.map(int.parse)
        |> list.map(result.unwrap(_, 0))
      case new |> is_higher(old) {
        True ->
          dict.insert(acc, search_result.package_name, search_result.version)
        False -> acc
      }
    }
  }
}

pub fn update_search_results_filter(model: Model) {
  let default_key = default_search_key(model.submitted_input)
  let show_old = case model.show_old_packages {
    True -> fn(_) { True }
    False -> {
      let last_versions = case dict.get(model.search_results, default_key) {
        Error(_) -> dict.new()
        Ok(search_results) -> {
          case search_results {
            search_result.Start | search_result.InternalServerError ->
              dict.new()
            search_result.SearchResults(t, e, m, s, d, mods) -> {
              dict.new()
              |> list.fold(t, _, extract_package_version)
              |> list.fold(e, _, extract_package_version)
              |> list.fold(m, _, extract_package_version)
              |> list.fold(s, _, extract_package_version)
              |> list.fold(d, _, extract_package_version)
              |> list.fold(mods, _, extract_package_version)
            }
          }
        }
      }
      fn(a: search_result.SearchResult) {
        case dict.get(last_versions, a.package_name) {
          Error(_) -> False
          Ok(content) -> content == a.version
        }
      }
    }
  }
  let or_filters =
    [
      #(model.keep_functions, fn(s: search_result.SearchResult) {
        s.kind == kind.Function
      }),
      #(model.keep_types, fn(s: search_result.SearchResult) {
        s.kind == kind.TypeDefinition
      }),
      #(model.keep_aliases, fn(s: search_result.SearchResult) {
        s.kind == kind.TypeAlias
      }),
    ]
    |> list.filter(fn(a) { a.0 })
    |> list.map(pair.second)
  let and_filters =
    [
      #(model.keep_documented, fn(s: search_result.SearchResult) {
        string.length(s.documentation) > 0
      }),
    ]
    |> list.filter(fn(a) { a.0 })
    |> list.map(pair.second)
  let filter = fn(s) {
    case list.is_empty(or_filters) {
      True -> True
      False -> list.any(or_filters, function.apply1(_, s))
    }
    && case list.is_empty(and_filters) {
      True -> True
      False -> list.any(and_filters, function.apply1(_, s))
    }
    && show_old(s)
  }
  let key = search_key(model.submitted_input, model)
  case dict.get(model.search_results, default_key) {
    Error(_) -> model
    Ok(search_results) -> {
      let search_results = case search_results {
        search_result.Start | search_result.InternalServerError ->
          search_results
        search_result.SearchResults(t, e, m, s, d, mods) ->
          search_result.SearchResults(
            t |> list.filter(filter),
            e |> list.filter(filter),
            m |> list.filter(filter),
            case model.show_vector_search {
              False -> []
              True -> s |> list.filter(filter)
            },
            case model.show_documentation_search {
              False -> []
              True -> d |> list.filter(filter)
            },
            mods |> list.filter(filter),
          )
      }
      let index = compute_index(search_results)
      let view_cache = case search_results {
        search_result.Start | search_result.InternalServerError ->
          model.view_cache
        search_result.SearchResults(types, e, m, s, d, mods) ->
          cache.cache_search_results(
            model.submitted_input,
            index,
            types,
            e,
            m,
            s,
            d,
            mods,
          )
          |> dict.insert(model.view_cache, key, _)
      }
      Model(
        ..model,
        search_results: dict.insert(model.search_results, key, search_results),
        index: index,
        view_cache: view_cache,
      )
    }
  }
}

pub fn reset(model: Model) {
  Model(
    search_results: model.search_results,
    input: "",
    index: [],
    loading: False,
    view_cache: model.view_cache,
    route: router.Home,
    trendings: model.trendings,
    submitted_input: "",
    keep_functions: False,
    keep_types: False,
    keep_aliases: False,
    keep_documented: False,
    show_old_packages: False,
    show_documentation_search: False,
    show_vector_search: False,
  )
}

fn compute_index(search_results: SearchResults) -> Index {
  case search_results {
    search_result.Start | search_result.InternalServerError -> []
    search_result.SearchResults(types, exact, others, searches, docs, modules) -> {
      []
      |> insert_module_names(types)
      |> insert_module_names(exact)
      |> insert_module_names(others)
      |> insert_module_names(searches)
      |> insert_module_names(docs)
      |> insert_module_names(modules)
      |> list.map(fn(i) { pair.map_second(i, list.reverse) })
    }
  }
}

fn insert_module_names(index: Index, search_results: List(SearchResult)) {
  use acc, val <- list.fold(search_results, index)
  let key = #(val.package_name, val.version)
  list.key_find(acc, key)
  |> result.unwrap([])
  |> fn(i) { list.prepend(i, #(val.module_name, val.name)) }
  |> fn(i) { list.key_set(acc, key, i) }
}
