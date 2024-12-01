import gleam/dynamic.{type Dynamic}

@external(javascript, "../gloogle.ffi.mjs", "scrollTo")
pub fn scroll_to(element id: String) -> fn(dispatch) -> Nil

@external(javascript, "../gloogle.ffi.mjs", "subscribeIsMobile")
pub fn suscribe_is_mobile(callback: fn(Bool) -> Nil) -> Nil

@external(javascript, "../gloogle.ffi.mjs", "subscribeFocus")
pub fn subscribe_focus(callback: fn(Dynamic) -> Nil) -> Nil

@external(javascript, "../gloogle.ffi.mjs", "focus")
pub fn focus(on id: String, event event: a) -> Nil

@external(javascript, "../gloogle.ffi.mjs", "isActive")
pub fn is_active(element id: String) -> Bool

@external(javascript, "../gloogle.ffi.mjs", "blur")
pub fn blur() -> Nil

@external(javascript, "../gloogle.ffi.mjs", "updateTitle")
pub fn update_title(title: String) -> Nil

@external(javascript, "../gloogle.ffi.mjs", "eventKey")
pub fn key(event: Dynamic) -> Result(String, Nil)
