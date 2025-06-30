/// Reads an environment variable from `import.meta.env`.
/// Automatically prefixes the variable with `VITE_`, since the variable is not
/// loaded otherwise.
@external(javascript, "./vitools.ffi.mjs", "get_env")
pub fn get_env(name name: String) -> Result(String, Nil)

/// Reads `import.meta.env.BASE_URL`.
@external(javascript, "./vitools.ffi.mjs", "baseURL")
pub fn base_url() -> Result(String, Nil)
