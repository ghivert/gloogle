import gleam/erlang/process.{type Subject}
import gleam/option.{type Option}

pub type Msg {
  Find(Subject(Option(List(Int))), String)
  Add(String, Int)
}
