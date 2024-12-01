import birl
import gleam/dynamic
import gleam/json
import gleam/option
import gleam/pair
import gleam/result

pub type Analytics {
  Analytics(
    total_searches: Int,
    total_signatures: Int,
    total_indexed: Int,
    timeseries: List(#(Int, birl.Time)),
    ranked: List(Package),
    popular: List(Package),
  )
}

pub type Package {
  Package(
    name: String,
    repository: String,
    rank: Int,
    popularity: option.Option(Int),
  )
}

pub fn encode(analytics: Analytics) {
  json.object([
    #("total_searches", json.int(analytics.total_searches)),
    #("total_signatures", json.int(analytics.total_signatures)),
    #("total_indexed", json.int(analytics.total_indexed)),
    #("ranked", json.array(analytics.ranked, encode_package)),
    #("popular", json.array(analytics.popular, encode_package)),
    #("timeseries", {
      json.array(analytics.timeseries, fn(row) {
        json.object([
          #("count", json.int(row.0)),
          #("date", json.string(birl.to_iso8601(row.1))),
        ])
      })
    }),
  ])
}

pub fn decode(dyn) {
  dynamic.decode6(
    Analytics,
    dynamic.field("total_searches", dynamic.int),
    dynamic.field("total_signatures", dynamic.int),
    dynamic.field("total_indexed", dynamic.int),
    dynamic.field("timeseries", {
      dynamic.list(dynamic.decode2(
        pair.new,
        dynamic.field("count", dynamic.int),
        dynamic.field("date", fn(dyn) {
          dynamic.string(dyn)
          |> result.then(fn(t) {
            birl.parse(t)
            |> result.replace_error([])
          })
        }),
      ))
    }),
    dynamic.field("ranked", dynamic.list(decode_package)),
    dynamic.field("popular", dynamic.list(decode_package)),
  )(dyn)
}

pub fn encode_package(package: Package) {
  let Package(name:, repository:, rank:, popularity:) = package
  json.object([
    #("name", json.string(name)),
    #("repository", json.string(repository)),
    #("rank", json.int(rank)),
    #("popularity", json.nullable(popularity, json.int)),
  ])
}

pub fn decode_package(dyn) {
  dynamic.decode4(
    Package,
    dynamic.field("name", dynamic.string),
    dynamic.field("repository", dynamic.string),
    dynamic.field("rank", dynamic.int),
    dynamic.field("popularity", dynamic.optional(dynamic.int)),
  )(dyn)
}
