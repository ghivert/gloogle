import gleam/dynamic.{type Dynamic}
import gleam/javascript/promise.{type Promise}
import lustre/effect.{type Effect}

pub fn from_promise(promise promise: Promise(a)) {
  use dispatch <- effect.from()
  promise.tap(promise, fn(content) { dispatch(content) })
  Nil
}

pub fn from_failable_promise(
  promise promise: Promise(Result(a, b)),
  on_error from_error: fn(Dynamic) -> b,
) -> Effect(Result(a, b)) {
  use dispatch <- effect.from()
  promise
  |> promise.tap(fn(content) { dispatch(content) })
  |> promise.rescue(fn(error) {
    let response = Error(from_error(error))
    dispatch(response)
    response
  })
  Nil
}
