import gleam
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

pub fn from_string(level: String) -> Result(Level, Nil) {
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
