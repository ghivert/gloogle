import gleam/int
import gleam/string
import lustre/element/html
import sketch
import sketch/size.{px}
import grille_pain/internals/data/toast.{type Level, type Toast}
import grille_pain/internals/view/colors

pub fn view(toast: Toast) {
  html.div(
    [
      pb_play_state(toast.running),
      pb_background_color(toast.level),
      pb_animation(toast.animation_duration),
      pb_base(),
    ],
    [],
  )
}

fn pb_base() {
  [sketch.animation_fill_mode("forwards"), sketch.height(px(5))]
  |> sketch.class()
  |> sketch.to_lustre()
}

fn pb_animation(duration: Int) {
  let duration_ = int.to_string(duration / 1000)
  [sketch.animation(duration_ <> "s linear 0s progress_bar")]
  |> sketch.dynamic("toast-duration-" <> duration_, _)
  |> sketch.to_lustre()
}

fn pb_background_color(level: Level) {
  let back_color = colors.progress_bar_from_level(level)
  let background = "var(--grille_pain-info-progress-bar, " <> back_color <> ")"
  string.join(["grille_pain", "pb", "background", back_color], "-")
  |> sketch.dynamic([sketch.background(background)])
  |> sketch.to_lustre()
}

fn pb_play_state(running: Bool) {
  let running_str = toast.running_to_string(running)
  string.join(["grille_pain", "pb", "play-state", running_str], "-")
  |> sketch.dynamic([sketch.animation_play_state(running_str)])
  |> sketch.to_lustre()
}
