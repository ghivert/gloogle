import backend/error.{type Error}
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
/// When the `work` function returns an error it is printed.
pub fn retry(
  do work: fn(Int) -> Result(a, Error),
) -> Result(Subject(Message), actor.StartError) {
  fn() { init(one_minute, work) }
  |> actor.Spec(loop: loop, init_timeout: 100)
  |> actor.start_spec()
}

fn init(
  interval: Int,
  work: fn(Int) -> Result(a, Error),
) -> actor.InitResult(State(a), Message) {
  let subject = process.new_subject()
  let random_ints = random.to_random_iterator(random.int(1000, 5000))
  let state =
    State(
      self: subject,
      work: work,
      interval: interval,
      iterations: 10,
      random_ints: random_ints,
    )
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
              wisp.log_notice("Stopping process after 10 iterations")
              actor.Stop(process.Normal)
            }
            False -> {
              State(..state, iterations: state.iterations - 1)
              |> enqueue_next_rerun()
              |> actor.continue()
            }
          }
        }
      }
    }
  }
}
