import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import lustre/effect.{type Effect}
import tardis/internals/data/msg.{type Msg}
import tardis/internals/data/step.{type Step, Step}

pub type Debuggers =
  List(#(String, Debugger))

pub type Debugger {
  Debugger(
    count: Int,
    steps: List(Step),
    dispatcher: Option(fn(Dynamic) -> Effect(Msg)),
    selected_step: Option(String),
  )
}

pub fn init() {
  Debugger(1, steps: [], dispatcher: None, selected_step: option.None)
}

pub fn add_dispatcher(debugger_: Debugger, dispatcher) {
  Debugger(..debugger_, dispatcher: Some(dispatcher))
}

pub fn replace(
  debuggers: Debuggers,
  debugger_: String,
  mapper: fn(Debugger) -> Debugger,
) -> Debuggers {
  debuggers
  |> list.find(fn(item) { pair.first(item) == debugger_ })
  |> result.unwrap(#(debugger_, init()))
  |> pair.second()
  |> mapper()
  |> list.key_set(debuggers, debugger_, _)
}

pub fn get(debuggers: Debuggers, debugger_: String) {
  debuggers
  |> list.find(fn(item) { pair.first(item) == debugger_ })
  |> result.map(pair.second)
}

pub fn unselect(debugger_: Debugger) {
  Debugger(..debugger_, selected_step: option.None)
}

pub fn select(debugger_: Debugger, step: Option(String)) {
  Debugger(..debugger_, selected_step: step)
}

pub fn add_step(debugger_: Debugger, model: Dynamic, msg: Dynamic) {
  let count = debugger_.count
  let steps = debugger_.steps
  let step = Step(int.to_string(count), model, msg)
  Debugger(..debugger_, count: count + 1, steps: [step, ..steps])
}
