pub fn tap(a: a, next: fn(a) -> b) -> a {
  next(a)
  a
}

pub fn identity(a: a) -> a {
  a
}
