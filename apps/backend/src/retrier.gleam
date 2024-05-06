import backend/error.{type Error}
import gleam/erlang/process.{type Subject}
import gleam/function
import gleam/otp/actor
import wisp

pub opaque type Message {
  Rerun
}

type State(a) {
  State(
    self: Subject(Message),
    work: fn(Int) -> Result(a, Error),
    interval: Int,
    iterations: Int,
  )
}

pub const ten_minutes: Int = 600_000

fn enqueue_next_rerun(state: State(a)) {
  process.send_after(state.self, state.interval, Rerun)
}

/// Repeatedly call a function, leaving `interval` milliseconds between each call.
/// When the `work` function returns an error it is printed.
pub fn retry(
  do work: fn(Int) -> Result(a, Error),
) -> Result(Subject(Message), actor.StartError) {
  fn() { init(2 * ten_minutes, work) }
  |> actor.Spec(loop: loop, init_timeout: 100)
  |> actor.start_spec()
}

fn init(
  interval: Int,
  work: fn(Int) -> Result(a, Error),
) -> actor.InitResult(State(a), Message) {
  let subject = process.new_subject()
  let state = State(subject, work, interval, 10)
  process.new_selector()
  |> process.selecting(subject, function.identity)
  |> actor.Ready(state, _)
  |> function.tap(fn(_) { process.send(state.self, Rerun) })
}

fn loop(message: Message, state: State(a)) -> actor.Next(Message, State(a)) {
  case message {
    Rerun -> {
      case state.work(state.iterations) {
        Ok(_) -> actor.Stop(process.Normal)
        Error(error) -> {
          wisp.log_notice("Process on error")
          error.log_error(error)
          case state.iterations == 0 {
            True -> {
              wisp.log_notice("Stopping process after 20 iterations")
              actor.Stop(process.Normal)
            }
            False -> {
              let new_state = State(..state, iterations: state.iterations - 1)
              enqueue_next_rerun(new_state)
              actor.continue(new_state)
            }
          }
        }
      }
    }
  }
}
