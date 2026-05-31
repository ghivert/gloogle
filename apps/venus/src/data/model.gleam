import bright
import data/analytics
import data/kind
import data/msg.{type Msg}
import data/package.{type Package}
import data/search_result.{type SearchResults, SearchResults}
import data/type_search.{type TypeSearch}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option}
import gleam/pair
import gleam/regexp
import gleam/result
import gleam/string
import gleam/time/timestamp
import lustre/element.{type Element}
import venus/router
import venus/view/body/cache

pub type Index =
  List(#(#(String, String), List(#(String, String))))

pub type Model =
  bright.Bright(State, Computed)

pub type State {
  State(
    input: String,
    search_results: Dict(String, SearchResults),
    index: Index,
    loading: Bool,
    packages: List(package.Package),
    view_cache: Dict(String, Element(Msg)),
    route: router.Route,
    is_mobile: Bool,
    trendings: Option(List(Package)),
    submitted_input: String,
    keep_functions: Bool,
    keep_types: Bool,
    keep_aliases: Bool,
    keep_documented: Bool,
    show_old_packages: Bool,
    show_documentation_search: Bool,
    show_vector_search: Bool,
    total_searches: Int,
    total_signatures: Int,
    total_packages: Int,
    timeseries: List(#(Int, timestamp.Timestamp)),
    ranked: List(analytics.Package),
    popular: List(analytics.Package),
  )
}

pub type Computed {
  Computed
}

@external(javascript, "../gloogle.ffi.mjs", "isMobile")
fn is_mobile() -> Bool

pub fn init_state() {
  let search_results = search_result.Start
  let index = compute_index(search_results)
  State(
    input: "",
    search_results: dict.new(),
    index: index,
    loading: False,
    packages: [],
    view_cache: dict.new(),
    route: router.Home,
    is_mobile: is_mobile(),
    trendings: option.None,
    submitted_input: "",
    keep_functions: False,
    keep_types: False,
    keep_aliases: False,
    keep_documented: False,
    show_old_packages: False,
    show_documentation_search: False,
    show_vector_search: False,
    total_searches: 0,
    total_signatures: 0,
    total_packages: 0,
    timeseries: [],
    ranked: [],
    popular: [],
  )
}

pub fn update_route(state: State, route: router.Route) {
  State(..state, route: route)
}

pub fn update_submitted_input(state: State) {
  State(..state, submitted_input: state.input)
}

pub fn update_is_mobile(state: State, is_mobile: Bool) {
  State(..state, is_mobile: is_mobile)
}

pub fn update_trendings(state: State, trendings: List(Package)) {
  state.trendings
  |> option.unwrap([])
  |> list.append(trendings)
  |> option.Some
  |> fn(t) { State(..state, trendings: t) }
}

pub fn toggle_loading(state: State) {
  State(..state, loading: !state.loading)
}

pub fn update_input(state: State, content: String) {
  State(..state, input: content)
}

pub fn update_analytics(state: State, analytics: analytics.Analytics) {
  State(
    ..state,
    timeseries: analytics.timeseries,
    total_searches: analytics.total_searches,
    total_signatures: analytics.total_signatures,
    total_packages: analytics.total_indexed,
    ranked: analytics.ranked,
    popular: analytics.popular,
  )
}

pub fn search_key(key key: String, state state: State) {
  key
  <> string.inspect([
    state.keep_functions,
    state.keep_types,
    state.keep_aliases,
    state.keep_documented,
    state.show_old_packages,
    state.show_documentation_search,
    state.show_vector_search,
  ])
}

fn default_search_key(key key: String) {
  key <> string.inspect([False, False, False, False, True, True, True])
}

pub fn update_search_results(
  state: State,
  key: String,
  search_results: SearchResults,
) {
  let key = default_search_key(key: key)
  let index = compute_index(search_results)
  let view_cache = case search_results {
    search_result.Start | search_result.InternalServerError -> state.view_cache
    SearchResults(types, e, m, s, d, mods) ->
      cache.cache_search_results(
        state.submitted_input,
        index,
        types,
        e,
        m,
        s,
        d,
        mods,
      )
      |> dict.insert(state.view_cache, key, _)
  }
  State(
    ..state,
    search_results: dict.insert(state.search_results, key, search_results),
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
  search_result: TypeSearch,
) -> Dict(String, String) {
  let assert Ok(re) = regexp.from_string("^[0-9]*.[0-9]*.[0-9]*$")
  case regexp.check(re, search_result.version) {
    False -> acc
    True ->
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
              dict.insert(
                acc,
                search_result.package_name,
                search_result.version,
              )
            False -> acc
          }
        }
      }
  }
}

pub fn update_search_results_filter(state: State) {
  let default_key = default_search_key(state.submitted_input)
  let show_old = case state.show_old_packages {
    True -> fn(_) { True }
    False -> {
      let last_versions = case dict.get(state.search_results, default_key) {
        Error(_) -> dict.new()
        Ok(search_results) -> {
          case search_results {
            search_result.Start | search_result.InternalServerError ->
              dict.new()
            SearchResults(t, e, m, s, d, mods) -> {
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
      fn(a: TypeSearch) {
        case dict.get(last_versions, a.package_name) {
          Error(_) -> False
          Ok(content) -> content == a.version
        }
      }
    }
  }
  let or_filters =
    [
      #(state.keep_functions, fn(s: TypeSearch) {
        s.signature_kind == kind.Function
      }),
      #(state.keep_types, fn(s: TypeSearch) {
        s.signature_kind == kind.TypeDefinition
      }),
      #(state.keep_aliases, fn(s: TypeSearch) {
        s.signature_kind == kind.TypeAlias
      }),
    ]
    |> list.filter(fn(a) { a.0 })
    |> list.map(pair.second)
  let and_filters =
    [
      #(state.keep_documented, fn(s: TypeSearch) {
        string.length(s.documentation) > 0
      }),
    ]
    |> list.filter(fn(a) { a.0 })
    |> list.map(pair.second)
  let filter = fn(s) {
    case list.is_empty(or_filters) {
      True -> True
      False -> list.any(or_filters, fn(f) { f(s) })
    }
    && case list.is_empty(and_filters) {
      True -> True
      False -> list.any(and_filters, fn(f) { f(s) })
    }
    && show_old(s)
  }
  let key = search_key(state.submitted_input, state)
  case dict.get(state.search_results, default_key) {
    Error(_) -> state
    Ok(search_results) -> {
      let search_results = case search_results {
        search_result.Start | search_result.InternalServerError ->
          search_results
        SearchResults(t, e, m, s, d, mods) ->
          SearchResults(
            t |> list.filter(filter),
            e |> list.filter(filter),
            m |> list.filter(filter),
            case state.show_vector_search {
              False -> []
              True -> s |> list.filter(filter)
            },
            case state.show_documentation_search {
              False -> []
              True -> d |> list.filter(filter)
            },
            mods |> list.filter(filter),
          )
      }
      let index = compute_index(search_results)
      let view_cache = case search_results {
        search_result.Start | search_result.InternalServerError ->
          state.view_cache
        SearchResults(types, e, m, s, d, mods) ->
          cache.cache_search_results(
            state.submitted_input,
            index,
            types,
            e,
            m,
            s,
            d,
            mods,
          )
          |> dict.insert(state.view_cache, key, _)
      }
      State(
        ..state,
        search_results: dict.insert(state.search_results, key, search_results),
        index: index,
        view_cache: view_cache,
      )
    }
  }
}

pub fn reset(state: State) {
  State(
    search_results: state.search_results,
    input: "",
    index: [],
    loading: False,
    view_cache: state.view_cache,
    packages: state.packages,
    route: router.Home,
    is_mobile: is_mobile(),
    trendings: state.trendings,
    submitted_input: "",
    keep_functions: False,
    keep_types: False,
    keep_aliases: False,
    keep_documented: False,
    show_old_packages: False,
    show_documentation_search: False,
    show_vector_search: False,
    timeseries: state.timeseries,
    total_searches: state.total_searches,
    total_signatures: state.total_signatures,
    total_packages: state.total_packages,
    ranked: state.ranked,
    popular: state.popular,
  )
}

fn compute_index(search_results: SearchResults) -> Index {
  case search_results {
    search_result.Start | search_result.InternalServerError -> []
    SearchResults(types, exact, others, searches, docs, modules) -> {
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

fn insert_module_names(index: Index, search_results: List(TypeSearch)) {
  use acc, val <- list.fold(search_results, index)
  let key = #(val.package_name, val.version)
  list.key_find(acc, key)
  |> result.unwrap([])
  |> fn(i) { list.prepend(i, #(val.module_name, val.type_name)) }
  |> fn(i) { list.key_set(acc, key, i) }
}
