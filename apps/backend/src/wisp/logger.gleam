import envoy
import gleam
import gleam/result
import gleam/string

pub type Level {
  Emergency
  Alert
  Critical
  Error
  Warning
  Notice
  Info
  Debug
}

@external(erlang, "backend_ffi", "set_level")
pub fn set_level(level: Level) -> Nil

pub fn read_level() {
  envoy.get("LOG_LEVEL")
  |> result.try(parse)
  |> result.unwrap(Info)
}

fn parse(level: String) -> Result(Level, Nil) {
  case string.lowercase(level) {
    "emergency" -> Ok(Emergency)
    "alert" -> Ok(Alert)
    "critical" -> Ok(Critical)
    "error" -> Ok(Error)
    "warning" -> Ok(Warning)
    "notice" -> Ok(Notice)
    "info" -> Ok(Info)
    "debug" -> Ok(Debug)
    _ -> gleam.Error(Nil)
  }
}
