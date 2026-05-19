import gleam/erlang/process.{type Subject}
import gleam/function_
import gleam/otp/actor
import gleam/result
import gleam/time/duration.{type Duration}
import jupiter/error.{type Error}

pub opaque type Message {
  Rerun
}

type State(a) {
  State(
    self: Subject(Message),
    work: fn() -> Result(a, Error),
    interval: Duration,
  )
}

fn enqueue_next_rerun(state: State(a)) {
  let interval = duration.to_milliseconds(state.interval)
  process.send_after(state.self, interval, Rerun)
}

/// Repeatedly call a function, leaving `interval` milliseconds between each call.
/// When the `work` function returns an error it is printed.
pub fn periodically(
  do work: fn() -> Result(a, Error),
  waiting interval: Duration,
) -> Result(actor.Started(Subject(Message)), actor.StartError) {
  init(interval, work, _)
  |> actor.new_with_initialiser(100, _)
  |> actor.on_message(loop)
  |> actor.start
}

fn init(interval: Duration, work: fn() -> Result(a, Error), subject) {
  let state = State(subject, work, interval)
  actor.initialised(state)
  |> actor.returning(subject)
  |> function_.tap(fn(_) { process.send(state.self, Rerun) })
  |> Ok
}

fn loop(state: State(a), message: Message) -> actor.Next(State(a), Message) {
  case message {
    Rerun -> {
      let _ = result.map_error(state.work(), fn(a) { echo a })
      enqueue_next_rerun(state)
      actor.continue(state)
    }
  }
}
