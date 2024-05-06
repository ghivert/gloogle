import grille_pain/internals/data/msg.{NewToast}
import grille_pain/internals/data/toast.{
  type Level, Error, Info, Standard, Success, Warning,
}
import grille_pain/internals/ffi
import lustre

fn dispatch_toast(content: String, level: Level) {
  let grille_pain_dispatch = ffi.dispatcher()
  NewToast(content, level)
  |> lustre.dispatch()
  |> grille_pain_dispatch()
}

pub fn info(content: String) {
  dispatch_toast(content, Info)
}

pub fn success(content: String) {
  dispatch_toast(content, Success)
}

pub fn error(content: String) {
  dispatch_toast(content, Error)
}

pub fn toast(content: String) {
  dispatch_toast(content, Standard)
}

pub fn warning(content: String) {
  dispatch_toast(content, Warning)
}
