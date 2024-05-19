import grille_pain/internals/data/msg.{type Msg}
import lustre.{type Action, type ClientSpa}
import plinth/browser/shadow.{type ShadowRoot}

pub type Dispatch =
  fn(Action(Msg, ClientSpa)) -> Nil

@external(javascript, "../../grille_pain.ffi.mjs", "storeDispatcher")
pub fn store_dispatcher(dispatcher: Dispatch) -> Dispatch {
  dispatcher
}

@external(javascript, "../../grille_pain.ffi.mjs", "getDispatcher")
pub fn dispatcher() -> Dispatch {
  fn(_) { Nil }
}

@external(javascript, "../../grille_pain.ffi.mjs", "isDarkTheme")
pub fn is_dark_theme() -> Bool {
  False
}

@external(javascript, "../../grille_pain.ffi.mjs", "computeToastSize")
pub fn compute_toast_size(id: Int) -> Int

@external(javascript, "../../grille_pain.ffi.mjs", "addKeyframe")
pub fn add_keyframe(root: ShadowRoot) -> Nil
