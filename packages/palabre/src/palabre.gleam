import gleam/float
import gleam/function
import gleam/http
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import palabre/internals/utils
import palabre/level
import wisp.{type Request, type Response}

pub opaque type Options {
  Options(color: Option(Bool), json: Bool, level: level.Level)
}

pub fn options() {
  Options(color: None, json: False, level: level.Info)
}

pub fn color(options, color) {
  Options(..options, color: Some(color))
}

pub fn json(options, json) {
  Options(..options, json:)
}

pub fn level(options, level) {
  Options(..options, level:)
}

@external(erlang, "palabre_ffi", "configure")
pub fn configure(options: Options) -> Nil

pub opaque type Log {
  Log(
    level: level.Level,
    fields: List(#(String, List(String))),
    message: Option(String),
  )
}

pub fn emergency() {
  init(level.Emergency)
}

pub fn alert() {
  init(level.Alert)
}

pub fn critical() {
  init(level.Critical)
}

pub fn error() {
  init(level.Error)
}

pub fn warning() {
  init(level.Warning)
}

pub fn notice() {
  init(level.Notice)
}

pub fn info() {
  init(level.Info)
}

pub fn debug() {
  init(level.Debug)
}

pub fn message(log: Log, message: String) {
  Log(..log, message: Some(message))
}

pub fn at(log: Log, module module: String, function function: String) {
  let key = "at"
  let is_json = utils.is_json()
  let is_color = utils.is_color()
  let #(module, separator, function, reset) = case is_json, is_color {
    False, False | True, _ -> #(module, ".", function, "")
    False, True -> #(
      "\u{1b}[35m" <> module,
      "\u{1b}[0m.",
      "\u{1b}[34m" <> function,
      "\u{1b}[0m",
    )
  }
  log.fields
  |> append_field(key, module <> separator <> function <> reset)
  |> set_fields(log)
}

pub fn string(log: Log, key: String, value: String) {
  log.fields
  |> append_field(key, value)
  |> set_fields(log)
}

pub fn int(log: Log, key: String, value: Int) {
  log.fields
  |> append_field(key, int.to_string(value))
  |> set_fields(log)
}

pub fn float(log: Log, key: String, value: Float) {
  log.fields
  |> append_field(key, float.to_string(value))
  |> set_fields(log)
}

pub fn dump(log_: Log) -> Nil {
  let text =
    log_.message
    |> case utils.is_color() {
      True -> option.map(_, fn(m) { "\u{1b}[1m" <> m <> "\u{1b}[0m" })
      False -> function.identity
    }
    |> option.unwrap("")
  case log_.level {
    level.Emergency -> log(log_.level, log_.fields, text)
    level.Alert -> log(log_.level, log_.fields, text)
    level.Critical -> log(log_.level, log_.fields, text)
    level.Error -> log(log_.level, log_.fields, text)
    level.Warning -> log(log_.level, log_.fields, text)
    level.Notice -> log(log_.level, log_.fields, text)
    level.Info -> log(log_.level, log_.fields, text)
    level.Debug -> log(log_.level, log_.fields, text)
  }
}

@external(erlang, "palabre_ffi", "log")
fn log(level: level.Level, message: a, text: String) -> Nil

fn init(level: level.Level) {
  Log(level:, fields: [], message: None)
}

fn append_field(
  fields: List(#(String, List(String))),
  key: String,
  value: String,
) -> List(#(String, List(String))) {
  fields
  |> list.key_find(key)
  |> result.unwrap([])
  |> list.prepend(value)
  |> list.key_set(fields, key, _)
}

pub fn log_request(req: Request, handler: fn() -> Response) -> Response {
  let response = handler()
  info()
  |> int("status", response.status)
  |> string("method", string.uppercase(http.method_to_string(req.method)))
  |> string("where", req.path)
  |> dump
  response
}

fn set_fields(fields: List(#(String, List(String))), log: Log) -> Log {
  Log(..log, fields:)
}
