@external(javascript, "../gloogle.ffi.mjs", "scrollTo")
pub fn scroll_to(element id: String) -> fn(dispatch) -> Nil

@external(javascript, "../gloogle.ffi.mjs", "updateTitle")
pub fn update_title(title: String) -> Nil
