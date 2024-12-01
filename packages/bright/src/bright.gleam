import gleam/bool
import gleam/dynamic.{type Dynamic}
import gleam/function
import gleam/list
import gleam/pair
import lustre/effect.{type Effect}

@external(erlang, "bright_ffi", "coerce")
@external(javascript, "./bright.ffi.mjs", "coerce")
fn coerce(a: a) -> b

/// Optimization on JS, to ensure two data sharing the referential equality
/// will shortcut the comparison. Useful when performance are a thing in client
/// browser.
@external(javascript, "./bright.ffi.mjs", "areReferentiallyEqual")
fn are_referentially_equal(a: a, b: b) -> Bool {
  dynamic.from(a) == dynamic.from(b)
}

/// `Bright` holds raw data and computed data, and is used to compute caching.
/// `Bright` is instanciated using `init`, with initial data and computed data.
pub opaque type Bright(data, computed) {
  Bright(
    data: data,
    computed: computed,
    selections: List(Dynamic),
    past_selections: List(Dynamic),
    effects: List(Dynamic),
  )
}

/// Creates the initial `Bright`. `data` & `computed` should be initialised with
/// their correct empty initial state.
pub fn init(data data: data, computed computed: computed) {
  Bright(data:, computed:, selections: [], past_selections: [], effects: [])
}

pub fn start(
  bright: Bright(data, computed),
  next: fn(Bright(data, computed)) -> Bright(data, computed),
) -> #(Bright(data, computed), Effect(msg)) {
  let old_computations = bright.past_selections
  let new_data = next(bright)
  let all_effects = dynamic.from(new_data.effects) |> coerce |> list.reverse
  panic_if_different_computations_count(old_computations, new_data.selections)
  let past_selections = list.reverse(new_data.selections)
  Bright(..new_data, past_selections:, selections: [], effects: [])
  |> pair.new(effect.batch(all_effects))
}

/// Entrypoint for the update cycle. Use it a way to trigger the start of `Bright`
/// computations, and chain them with other `bright` calls.
///
/// ```gleam
/// pub fn update(model: Bright(data, computed), msg: Msg) {
///   // Starts the update cycle, and returns #(Bright(data, computed), Effect(msg)).
///   use model <- bright.update(model, update_data(_, msg))
///   bright.return(model)
/// }
/// ```
pub fn update(
  bright: Bright(data, computed),
  update_: fn(data) -> #(data, Effect(msg)),
  next: fn(Bright(data, computed)) -> Bright(data, computed),
) -> Bright(data, computed) {
  let #(data, effects) = update_(bright.data)
  let effects = [dynamic.from(effects), ..bright.effects]
  Bright(..bright, data:, effects:)
  |> next
}

/// Derives data from the `data` state, and potentially the current `computed`
/// state. `compute` will run **at every render**, so be careful with computations
/// as they can block paint or actors.
///
/// ```gleam
/// pub fn update(model: Bright(data, computed), msg: Msg) {
///   use model <- bright.update(model, update_data(_, msg))
///   model
///   |> bright.compute(fn (d, c) { Computed(..c, field1: computation1(d)) })
///   |> bright.compute(fn (d, c) { Computed(..c, field2: computation2(d)) })
///   |> bright.compute(fn (d, c) { Computed(..c, field3: computation3(d)) })
/// }
/// ```
pub fn compute(
  bright: Bright(data, computed),
  compute_: fn(data, computed) -> computed,
) -> Bright(data, computed) {
  compute_(bright.data, bright.computed)
  |> fn(computed) { Bright(..bright, computed:) }
}

/// Plugs in existing `data` and `computed` state, to issue some side-effects,
/// when your application needs to run side-effects depending on the current state.
///
/// ```gleam
/// pub fn update(model: Bright(data, computed), msg: Msg) {
///   use model <- bright.update(model, update_data(_, msg))
///   use d, c <- bright.guard(model)
///   use dispatch <- effect.from
///   case d.field == 10 {
///     True -> dispatch(my_msg)
///     False -> Nil
///   }
/// }
/// ```
pub fn run(
  bright: Bright(data, computed),
  guard_: fn(data, computed) -> Effect(msg),
) -> Bright(data, computed) {
  guard_(bright.data, bright.computed)
  |> dynamic.from
  |> list.prepend(bright.effects, _)
  |> fn(effects) { Bright(..bright, effects:) }
}

/// Derives data like [`compute`](#compute) lazily. `lazy_compute` accepts a
/// selector as second argument. Each time the selector returns a different data
/// than previous run, the computation will run. Otherwise, nothing happens.
///
/// ```gleam
/// pub fn update(model: Bright(data, computed), msg: Msg) {
///   use model <- bright.update(model, update_data(_, msg))
///   model
///   |> bright.lazy_compute(selector, fn (d, c) { Computed(..c, field1: computation1(d)) })
///   |> bright.lazy_compute(selector, fn (d, c) { Computed(..c, field2: computation2(d)) })
///   |> bright.lazy_compute(selector, fn (d, c) { Computed(..c, field3: computation3(d)) })
/// }
///
/// /// Use it with lazy_compute to recompute only when the field when
/// /// { old_data.field / 10 } != { data.field / 10 }
/// fn selector(d, _) {
///   d.field / 10
/// }
/// ```
pub fn lazy_compute(
  bright: Bright(data, computed),
  selector: fn(data) -> a,
  compute_: fn(data, computed) -> computed,
) -> Bright(data, computed) {
  lazy_wrap(bright, selector, compute, compute_)
}

/// Plugs in existing `data` like [`guard`](#guard) lazily. `lazy_guard` accepts
/// a selector as second argument. Each time the selector returns a different data
/// than previous run, the computation will run. Otherwise, nothing happens.
///
/// ```gleam
/// pub fn update(model: Bright(data, computed), msg: Msg) {
///   use model <- bright.update(model, update_data(_, msg))
///   use d, c <- bright.lazy_guard(model, selector)
///   use dispatch <- effect.from
///   case d.field == 10 {
///     True -> dispatch(my_msg)
///     False -> Nil
///   }
/// }
///
/// /// Use it with lazy_guard to recompute only when the field when
/// /// { old_data.field / 10 } != { data.field / 10 }
/// fn selector(d, _) {
///   d.field / 10
/// }
/// ```
pub fn lazy_run(
  bright: Bright(data, computed),
  selector: fn(data) -> a,
  guard_: fn(data, computed) -> Effect(msg),
) -> Bright(data, computed) {
  lazy_wrap(bright, selector, run, guard_)
}

/// Injects `Bright(data, computed)` in the `view` function, like a middleware.
/// Used to extract `data` & `computed` states from `Bright`.
///
/// ```gleam
/// pub fn view(model: Bright(data, computed)) {
///   use data, computed <- bright.view(model)
///   html.div([], [
///     // Use data or computed here.
///   ])
/// }
/// ```
pub fn view(
  bright: Bright(data, computed),
  viewer: fn(data, computed) -> a,
) -> a {
  viewer(bright.data, bright.computed)
}

/// Allows to run multiple `update` on multiple `Bright` in the same update cycle.
/// Every call to step with compute a new `Bright`, and will let you chain the
/// steps.
///
/// ```gleam
/// pub type Model {
///   Model(
///     fst_bright: Bright(data, computed),
///     snd_bright: Bright(data, computed),
///   )
/// }
///
/// fn update(model: Model, msg: Msg) {
///   use fst_bright <- bright.step(update_fst(model.fst_bright, msg))
///   use snd_bright <- bright.step(update_snd(model.snd_bright, msg))
///   bright.return(Model(fst_bright:, snd_bright:))
/// }
/// ```
pub fn step(
  bright: #(Bright(data, computed), Effect(msg)),
  next: fn(Bright(data, computed)) -> #(model, Effect(msg)),
) {
  let #(bright, effs) = bright
  let #(model, effs_) = next(bright)
  #(model, effect.batch([effs, effs_]))
}

/// Helper to write `bright` update cycle. Equivalent to `#(a, effect.none())`.
pub fn return(a) {
  #(a, effect.none())
}

fn lazy_wrap(
  bright: Bright(data, computed),
  selector: fn(data) -> a,
  setter: fn(Bright(data, computed), fn(data, computed) -> c) ->
    Bright(data, computed),
  compute_: fn(data, computed) -> c,
) -> Bright(data, computed) {
  let selected_data = selector(bright.data)
  let selections = [dynamic.from(selected_data), ..bright.selections]
  let bright = Bright(..bright, selections:)
  case bright.past_selections {
    [] -> setter(bright, compute_)
    [value, ..past_selections] -> {
      Bright(..bright, past_selections:)
      |> case are_referentially_equal(value, selected_data) {
        True -> function.identity
        False -> setter(_, compute_)
      }
    }
  }
}

fn panic_if_different_computations_count(
  old_computations: List(c),
  computations: List(d),
) -> Nil {
  let count = list.length(old_computations)
  use <- bool.guard(when: count == 0, return: Nil)
  let is_same_count = count == list.length(computations)
  use <- bool.guard(when: is_same_count, return: Nil)
  panic as "Memoized computed should be consistent over time, otherwise memo can not work."
}
