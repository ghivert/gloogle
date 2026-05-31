import data/analytics
import data/model.{type State}
import data/msg
import data/package
import data/search_result
import gleam/dynamic/decode
import lustre/effect
import venus/discuss

pub fn get_trendings() {
  use dispatch <- effect.from
  discuss.about(["trendings"])
  |> discuss.expect(decode.list(package.decoder()))
  |> discuss.on_success(fn(m) { dispatch(msg.ApiReturnedTrendings(m)) })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}

pub fn get_packages() {
  use dispatch <- effect.from
  discuss.about(["packages"])
  |> discuss.expect(decode.list(package.decoder()))
  |> discuss.on_success(fn(m) { dispatch(msg.ApiReturnedPackages(m)) })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}

pub fn get_analytics() {
  use dispatch <- effect.from
  discuss.about(["analytics"])
  |> discuss.expect(analytics.decoder())
  |> discuss.on_success(fn(m) { dispatch(msg.ApiReturnedAnalytics(m)) })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}

pub fn get_search(state: State) {
  use dispatch <- effect.from
  discuss.about(["search"])
  |> discuss.query([#("q", state.input)])
  |> discuss.expect(search_result.search_results_decoder())
  |> discuss.on_success(fn(search_results) {
    state.input
    |> msg.ApiReturnedSearchResults(input: _, search_results:)
    |> dispatch
  })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}
