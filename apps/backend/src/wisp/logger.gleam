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

@external(erlang, "gloogle_hex_ffi", "set_level")
pub fn set_level(level: Level) -> Nil
