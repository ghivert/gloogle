@external(javascript, "../scheduler_ffi.mjs", "setTimeout")
pub fn set_timeout(timeout: Int, callback: fn() -> Nil) -> Nil
