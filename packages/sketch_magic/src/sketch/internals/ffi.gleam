import gleam/dynamic.{type Dynamic}
import sketch

@external(javascript, "../../sketch_magic.ffi.mjs", "setCache")
pub fn set_cache(cache: sketch.Cache) -> sketch.Cache {
  cache
}

@external(javascript, "../../sketch_magic.ffi.mjs", "getCache")
pub fn get_cache() -> Result(sketch.Cache, Nil) {
  Error(Nil)
}

@external(javascript, "../../sketch_magic.ffi.mjs", "createCssStyleSheet")
pub fn create_document_stylesheet() -> Dynamic {
  dynamic.from(Nil)
}

@external(javascript, "../../sketch_magic.ffi.mjs", "createCssStyleSheet")
pub fn create_shadow_root_stylesheet(_root: Dynamic) -> Dynamic {
  dynamic.from(Nil)
}

@external(javascript, "../../sketch_magic.ffi.mjs", "setStylesheet")
pub fn set_stylesheet(_content: String, _stylesheet: Dynamic) -> Nil {
  Nil
}
