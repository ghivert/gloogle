pub fn tap(res: Result(a, b), tap_: fn(a) -> c) -> Result(a, b) {
  case res {
    Error(_) -> res
    Ok(value) -> {
      tap_(value)
      res
    }
  }
}
