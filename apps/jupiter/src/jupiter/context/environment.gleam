import envoy

pub type Environment {
  Development
  Production
}

pub fn read() {
  case envoy.get("GLEAM_ENV") {
    Ok("development") -> Development
    _ -> Production
  }
}

pub fn is_dev() {
  case read() {
    Development -> True
    Production -> False
  }
}
