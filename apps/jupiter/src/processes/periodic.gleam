import backend/error.{type Error}
import gleam/erlang/process.{type Subject}
import gleam/function
import gleam/io
import gleam/otp/actor
import gleam/result

pub opaque type Message {
  Rerun
}

type State(a) {
  State(self: Subject(Message), work: fn() -> Result(a, Error), interval: Int)
}

fn enqueue_next_rerun(state: State(a)) {
  process.send_after(state.self, state.interval, Rerun)
}

/// Repeatedly call a function, leaving `interval` milliseconds between each call.
/// When the `work` function returns an error it is printed.
pub fn periodically(
  do work: fn() -> Result(a, Error),
  waiting interval: Int,
) -> Result(Subject(Message), actor.StartError) {
  fn() { init(interval, work) }
  |> actor.Spec(loop: loop, init_timeout: 100)
  |> actor.start_spec()
}

fn init(
  interval: Int,
  work: fn() -> Result(a, Error),
) -> actor.InitResult(State(a), Message) {
  let subject = process.new_subject()
  let state = State(subject, work, interval)
  process.new_selector()
  |> process.selecting(subject, function.identity)
  |> actor.Ready(state, _)
  |> function.tap(fn(_) { process.send(state.self, Rerun) })
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
