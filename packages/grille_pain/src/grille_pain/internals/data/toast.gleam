import birl

pub type Toast {
  Toast(
    id: Int,
    content: String,
    displayed: Bool,
    running: Bool,
    remaining: Int,
    last_schedule: birl.Time,
    iteration: Int,
    bottom: Int,
    level: Level,
    animation_duration: Int,
  )
}

pub type Level {
  Standard
  Info
  Warning
  Error
  Success
}

@external(javascript, "../../../grille_pain.ffi.mjs", "computeBottomPosition")
fn compute_bottom_position() -> Int

pub fn new(id: Int, content: String, level: Level, animation_duration: Int) {
  Toast(
    id: id,
    content: content,
    displayed: False,
    running: False,
    remaining: animation_duration,
    last_schedule: birl.now(),
    iteration: 0,
    bottom: compute_bottom_position(),
    level: level,
    animation_duration: animation_duration,
  )
}

pub fn running_to_string(running: Bool) {
  case running {
    True -> "running"
    False -> "paused"
  }
}
