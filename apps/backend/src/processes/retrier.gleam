import backend/error.{type Error}
import gleam/bool
import gleam/erlang/process.{type Subject}
import gleam/function
import gleam/iterator.{type Iterator}
import gleam/otp/actor
import prng/random
import wisp

pub opaque type Message {
  Rerun
}

type State(a) {
  State(
    self: Subject(Message),
    work: fn(Int) -> Result(a, Error),
    random_ints: Iterator(Int),
    interval: Int,
    iterations: Int,
  )
}

pub const one_minute: Int = 60_000

fn enqueue_next_rerun(state: State(a)) {
  let assert iterator.Next(cooldown, acc) = iterator.step(state.random_ints)
  process.send_after(state.self, state.interval + cooldown, Rerun)
  State(..state, random_ints: acc)
}

/// Repeatedly call a function, leaving `interval` milliseconds between each call.
/// When the `work` function returns an error, it is printed.
pub fn retry(do work: fn(Int) -> Result(a, Error)) {
  let _ = start_retrier(work)
  Nil
}

fn start_retrier(work: fn(Int) -> Result(a, Error)) {
  fn() { init(every: one_minute, do: work) }
  |> actor.Spec(loop:, init_timeout: 100)
  |> actor.start_spec
}

fn init(
  every interval: Int,
  do work: fn(Int) -> Result(a, Error),
) -> actor.InitResult(State(a), Message) {
  let self = process.new_subject()
  let random_ints = random.to_random_iterator(random.int(1000, 5000))
  let state = State(self:, work:, interval:, iterations: 10, random_ints:)
  process.new_selector()
  |> process.selecting(self, function.identity)
  |> actor.Ready(state, _)
  |> function.tap(fn(_) { process.send(state.self, Rerun) })
}

fn loop(message: Message, state: State(a)) -> actor.Next(Message, State(a)) {
  case message, state.work(state.iterations) {
    Rerun, Ok(_) -> actor.Stop(process.Normal)
    Rerun, Error(error) -> {
      wisp.log_notice("Process on error")
      error.log_error(error)
      use <- bool.lazy_guard(when: state.iterations == 0, return: stop_process)
      State(..state, iterations: state.iterations - 1)
      |> enqueue_next_rerun()
      |> actor.continue()
    }
  }
}

fn stop_process() {
  wisp.log_notice("Stopping process after 10 iterations")
  actor.Stop(process.Normal)
}
