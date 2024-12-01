import lustre/effect

pub fn update_title(title: String) {
  use _ <- effect.from
  do_update_title(title)
}

@external(javascript, "../../gloogle.ffi.mjs", "updateTitle")
fn do_update_title(title: String) -> Nil
