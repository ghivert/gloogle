import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/function
import gleam/otp/actor
import gleam/result

// import packages/error.{type Error}

pub opaque type Message {
  Rerun
}

type State(a) {
  State(self: Subject(Message), work: fn() -> Result(a, Nil), interval: Int)
}

/// Repeatedly call a function, leaving `interval` milliseconds between each
/// call.
/// When the `work` function returns an error it is printed.
pub fn periodically(
  do work: fn() -> Result(a, Nil),
  waiting interval: Int,
) -> Result(Subject(Message), actor.StartError) {
  fn() { init(interval, work) }
  |> actor.Spec(loop: loop, init_timeout: 100)
  |> actor.start_spec()
}

fn init(
  interval: Int,
  work: fn() -> Result(a, Nil),
) -> actor.InitResult(State(a), Message) {
  let subject = process.new_subject()
  let state = State(subject, work, interval)
  process.new_selector()
  |> process.selecting(subject, function.identity)
  |> fn(selector) {
    enqueue_next_rerun(state)
    actor.Ready(state, selector)
  }
}

fn loop(message: Message, state: State(a)) -> actor.Next(Message, State(a)) {
  case message {
    Rerun -> {
      let _ = result.map_error(state.work(), io.debug)
      enqueue_next_rerun(state)
      actor.continue(state)
    }
  }
}

fn enqueue_next_rerun(state: State(a)) {
  process.send_after(state.self, state.interval, Rerun)
}
