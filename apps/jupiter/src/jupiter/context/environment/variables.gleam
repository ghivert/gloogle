import envoy
import gleam/int
import gleam/result

pub fn database_url() {
  read("DATABASE_URL")
}

pub fn hex_api_key() {
  read("HEX_API_KEY")
}

pub fn github_token() {
  read("GITHUB_TOKEN")
}

pub fn bucket_uri() {
  read("BUCKET_URI")
}

pub fn scaleway_keys() {
  let access_key = read("SCALEWAY_ACCESS_KEY")
  let secret_key = read("SCALEWAY_SECRET_KEY")
  #(access_key, secret_key)
}

pub fn log_level() {
  envoy.get("LOG_LEVEL")
  |> result.unwrap("INFO")
}

pub fn port() {
  envoy.get("PORT")
  |> result.try(int.parse)
  |> result.unwrap(3000)
}

fn read(value) {
  let assert Ok(value) = envoy.get(value)
  value
}
