import data/analytics
import data/model.{type Data}
import data/msg
import data/package
import data/search_result
import frontend/discuss
import gleam/dynamic
import lustre/effect

pub fn get_trendings() {
  use dispatch <- effect.from
  discuss.about(["trendings"])
  |> discuss.expect(dynamic.list(package.decode))
  |> discuss.on_success(fn(m) { dispatch(msg.ApiReturnedTrendings(m)) })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}

pub fn get_packages() {
  use dispatch <- effect.from
  discuss.about(["packages"])
  |> discuss.expect(dynamic.list(package.decode))
  |> discuss.on_success(fn(m) { dispatch(msg.ApiReturnedPackages(m)) })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}

pub fn get_analytics() {
  use dispatch <- effect.from
  discuss.about(["analytics"])
  |> discuss.expect(analytics.decode)
  |> discuss.on_success(fn(m) { dispatch(msg.ApiReturnedAnalytics(m)) })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}

pub fn get_search(data: Data) {
  use dispatch <- effect.from
  discuss.about(["search"])
  |> discuss.query([#("q", data.input)])
  |> discuss.expect(search_result.decode_search_results)
  |> discuss.on_success(fn(search_results) {
    data.input
    |> msg.ApiReturnedSearchResults(input: _, search_results:)
    |> dispatch
  })
  |> discuss.on_error(fn(e) { dispatch(msg.AppRequiredDiscussToast(e)) })
  |> discuss.start
  Nil
}
