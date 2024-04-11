import gleam/dynamic.{type Dynamic}
import lustre/effect.{type Effect}
import tardis/internals/data/step.{type Step}
import tardis/internals/data/colors.{type ColorScheme}

pub type Msg {
  // Panel
  ToggleOpen
  UpdateColorScheme(ColorScheme)
  Debug(String)
  SelectDebugger(String)
  // Debugger
  AddApplication(String, fn(Dynamic) -> Effect(Msg))
  AddStep(String, Dynamic, Dynamic)
  BackToStep(String, Step)
  Restart(String)
}
