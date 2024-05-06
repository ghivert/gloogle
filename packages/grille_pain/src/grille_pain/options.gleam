import gleam/option.{type Option, None, Some}
import tardis

pub type Options {
  Options(timeout: Int, debug: Option(tardis.Tardis))
}

pub fn default() -> Options {
  Options(timeout: 5000, debug: None)
}

pub fn debug(options, debug) {
  Options(..options, debug: Some(debug))
}

pub fn timeout(options: Options, timeout: Int) -> Options {
  Options(..options, timeout: timeout)
}
