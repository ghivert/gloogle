import decrypt
import gleam/dynamic/decode
import gleam/json
import gleam/option
import gleam/time/calendar
import gleam/time/timestamp

pub type Analytics {
  Analytics(
    total_searches: Int,
    total_signatures: Int,
    total_indexed: Int,
    timeseries: List(#(Int, timestamp.Timestamp)),
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
      use #(count, date) <- json.array(analytics.timeseries)
      json.object([
        #("count", json.int(count)),
        #("date", json.string(timestamp.to_rfc3339(date, calendar.utc_offset))),
      ])
    }),
  ])
}

pub fn decoder() {
  use total_searches <- decode.field("total_searches", decode.int)
  use total_signatures <- decode.field("total_signatures", decode.int)
  use total_indexed <- decode.field("total_indexed", decode.int)
  use ranked <- decode.field("ranked", decode.list(package_decoder()))
  use popular <- decode.field("popular", decode.list(package_decoder()))
  use timeseries <- decode.field("timeseries", {
    decode.list({
      use count <- decode.field("count", decode.int)
      use date <- decode.field("date", decrypt.timestamp())
      decode.success(#(count, date))
    })
  })
  decode.success({
    Analytics(
      total_searches:,
      total_signatures:,
      total_indexed:,
      timeseries:,
      ranked:,
      popular:,
    )
  })
}

pub fn encode_package(package: Package) {
  json.object([
    #("name", json.string(package.name)),
    #("repository", json.string(package.repository)),
    #("rank", json.int(package.rank)),
    #("popularity", json.nullable(package.popularity, json.int)),
  ])
}

pub fn package_decoder() {
  use name <- decode.field("name", decode.string)
  use repository <- decode.field("repository", decode.string)
  use rank <- decode.field("rank", decode.int)
  use popularity <- decode.field("popularity", decode.optional(decode.int))
  decode.success(Package(name:, repository:, rank:, popularity:))
}
