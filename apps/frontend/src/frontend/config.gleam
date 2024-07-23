import vitools

pub fn api_endpoint() {
  case vitools.is_dev() {
    True -> "http://localhost:3000"
    False -> "https://api.gloogle.run"
  }
}
