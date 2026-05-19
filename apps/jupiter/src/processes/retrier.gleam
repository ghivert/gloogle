import gleam/bool
import gleam/erlang/process.{type Subject}
import gleam/float
import gleam/function_
import gleam/otp/actor
import gleam/time/timestamp
import jupiter/error.{type Error}
import prng/random
import wisp

pub opaque type Message {
  Rerun
}

type State(a) {
  State(
    self: Subject(Message),
    work: fn(Int) -> Result(a, Error),
    random_ints: #(random.Generator(Int), random.Seed),
    interval: Int,
    iterations: Int,
  )
}

pub const one_minute: Int = 3_600_000

fn enqueue_next_rerun(state: State(a)) {
  let #(random_ints, seed) = state.random_ints
  let #(cooldown, seed) = random.step(random_ints, seed)
  process.send_after(state.self, state.interval + cooldown, Rerun)
  State(..state, random_ints: #(random_ints, seed))
}

/// Call a function and retry its execution while it returns an error.
/// When the function returns `Ok`, the actor will stop.
/// Whenever the function returns an error, the error is printed.
/// Each call is spaced `interval` milliseconds apart.
pub fn retry(do work: fn(Int) -> Result(a, Error)) {
  init(_, every: one_minute, do: work)
  |> actor.new_with_initialiser(100, _)
  |> actor.on_message(loop)
  |> actor.start
}

fn init(
  self: Subject(Message),
  every interval: Int,
  do work: fn(Int) -> Result(a, Error),
) {
  let seed =
    timestamp.system_time()
    |> timestamp.to_unix_seconds
    |> float.round
    |> random.new_seed
  let random_ints = #(random.int(1000, 5000), seed)
  let state = State(self:, work:, interval:, iterations: 10, random_ints:)
  actor.initialised(state)
  |> actor.returning(self)
  |> function_.tap(fn(_) { process.send(state.self, Rerun) })
  |> Ok
}

fn loop(state: State(a), message: Message) -> actor.Next(State(a), Message) {
  case message, state.work(state.iterations) {
    Rerun, Ok(_) -> actor.stop()
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
  actor.stop()
}
