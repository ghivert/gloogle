/// Mode used at runtime. Any mode can be set with the CLI, by using
/// `--mode [mode-name]`. Steerlab only supports `development`, `staging`
/// and `production`.
pub type Mode {
  Production
  Staging
  Development
}

/// Read the mode set at build or runtime. Any mode can be set with the CLI,
/// by using `--mode [mode-name]`. Steerlab only supports `development`,
/// `staging` and `production`.
///
/// Defaults to `Production` if nothing can be read.
pub fn mode() -> Mode {
  case read() {
    Ok("development") -> Development
    Ok("staging") -> Staging
    Ok("production") -> Production
    Ok(_) -> Production
    Error(_) -> Production
  }
}

/// `True` when running in development, with the webserver.
pub fn dev() -> Bool {
  case mode() {
    Development -> True
    Production -> False
    Staging -> False
  }
}

/// Reads `import.meta.env.MODE`.
@external(javascript, "./mode.ffi.mjs", "read")
fn read() -> Result(String, Nil)
