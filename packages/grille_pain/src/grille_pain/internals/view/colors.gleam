import grille_pain/internals/data/toast.{
  type Level, Error, Info, Standard, Success, Warning,
}
import grille_pain/internals/ffi

pub const info = "#3498db"

pub const success = "#07bc0c"

pub const warning = "#f1c40f"

pub const error = "#e74c3c"

pub const color_transparent = "rgba(255, 255, 255, 0.7)"

pub const light_transparent = "rgb(0, 0, 0, 0.7)"

pub const dark_transparent = "rgb(255, 255, 255, 0.7)"

pub const dark = "#121212"

pub const light = "#fff"

fn from_standard() {
  case ffi.is_dark_theme() {
    True -> #(dark, light)
    False -> #(light, dark)
  }
}

pub fn from_level(level: Level) {
  case level {
    Standard -> from_standard()
    Info -> #(info, light)
    Success -> #(success, light)
    Warning -> #(warning, light)
    Error -> #(error, light)
  }
}

pub fn progress_bar_from_level(level: Level) {
  case level {
    Info | Success | Warning | Error -> color_transparent
    Standard ->
      case ffi.is_dark_theme() {
        True -> dark_transparent
        False -> light_transparent
      }
  }
}
