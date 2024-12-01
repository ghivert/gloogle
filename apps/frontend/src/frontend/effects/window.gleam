import data/msg
import gleam/bool
import gleam/dynamic.{type Dynamic}
import lustre/effect
import lustre/event

pub fn focus(on id: String, event event: Dynamic) {
  use _ <- effect.from()
  use <- bool.guard(when: do_is_active(id), return: Nil)
  event.prevent_default(event)
  do_focus(on: id, event: event)
}

pub fn blur() {
  use _ <- effect.from()
  do_blur()
}

pub fn subscribe_focus() {
  use dispatch <- effect.from()
  use event <- do_subscribe_focus()
  case do_key(event) {
    Ok("Escape") -> dispatch(msg.UserPressedEscape)
    _ -> dispatch(msg.UserFocusedSearch(event))
  }
}

pub fn subscribe_is_mobile() {
  use dispatch <- effect.from()
  use is_mobile <- do_suscribe_is_mobile()
  dispatch(msg.BrowserResizedViewport(is_mobile))
}

// FFI

@external(javascript, "../../gloogle.ffi.mjs", "subscribeFocus")
fn do_subscribe_focus(callback: fn(Dynamic) -> Nil) -> Nil

@external(javascript, "../../gloogle.ffi.mjs", "subscribeIsMobile")
fn do_suscribe_is_mobile(callback: fn(Bool) -> Nil) -> Nil

@external(javascript, "../../gloogle.ffi.mjs", "focus")
fn do_focus(on id: String, event event: a) -> Nil

@external(javascript, "../../gloogle.ffi.mjs", "isActive")
fn do_is_active(element id: String) -> Bool

@external(javascript, "../../gloogle.ffi.mjs", "eventKey")
fn do_key(event: Dynamic) -> Result(String, Nil)

@external(javascript, "../../gloogle.ffi.mjs", "blur")
fn do_blur() -> Nil
