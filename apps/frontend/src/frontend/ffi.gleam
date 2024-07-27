import plinth/browser/event.{type Event}

@external(javascript, "../gloogle.ffi.mjs", "scrollTo")
pub fn scroll_to(element id: String) -> fn(dispatch) -> Nil

@external(javascript, "../gloogle.ffi.mjs", "subscribeIsMobile")
pub fn suscribe_is_mobile(callback: fn(Bool) -> Nil) -> Nil

@external(javascript, "../gloogle.ffi.mjs", "subscribeFocus")
pub fn subscribe_focus(callback: fn(Event) -> Nil) -> Nil

@external(javascript, "../gloogle.ffi.mjs", "focus")
pub fn focus(on id: String, event event: a) -> Nil

@external(javascript, "../gloogle.ffi.mjs", "isActive")
pub fn is_active(element id: String) -> Bool

@external(javascript, "../gloogle.ffi.mjs", "unfocus")
pub fn unfocus() -> Nil

@external(javascript, "../gloogle.ffi.mjs", "updateTitle")
pub fn update_title(title: String) -> Nil
