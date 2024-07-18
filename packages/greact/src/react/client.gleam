@external(javascript, "./main.ffi.mjs", "createRoot")
fn create_root(root: String) -> Root

@external(javascript, "./main.ffi.mjs", "render")
fn render(root: Root, child: Component) -> Nil
