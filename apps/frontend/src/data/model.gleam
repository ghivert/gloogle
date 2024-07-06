import data/msg.{type Msg}
import data/package.{type Package}
import data/search_result.{type SearchResult, type SearchResults}
import frontend/router
import frontend/view/body/cache
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option}
import gleam/pair
import gleam/result
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
  )
}

pub fn update_route(model: Model, route: router.Route) {
  router.update_page_title(route)
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

pub fn update_search_results(
  model: Model,
  key: String,
  search_results: SearchResults,
) {
  let index = compute_index(search_results)
  let view_cache = case search_results {
    search_result.Start | search_result.InternalServerError -> model.view_cache
    search_result.SearchResults(types, e, m, s, d, mods) ->
      cache.cache_search_results(index, types, e, m, s, d, mods)
      |> dict.insert(model.view_cache, key, _)
  }
  Model(
    ..model,
    search_results: dict.insert(model.search_results, key, search_results),
    index: index,
    view_cache: view_cache,
  )
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
