import gleam/dynamic.{type Dynamic}
import gleam/javascript/promise.{type Promise}
import lustre/effect.{type Effect}

pub fn then(effect: Effect(a), next: fn(a) -> Effect(b)) -> Effect(b) {
  // This is necessary because `perform` needs an implementation for
  // `effect.emit` but we can't handle that. If you're not using components
  // this will never come up.
  let dummy_emit = fn(_, _) { Nil }
  use dispatch <- effect.from()
  let run_next = fn(a) { effect.perform(next(a), dispatch, dummy_emit) }
  effect.perform(effect, run_next, dummy_emit)
}

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
