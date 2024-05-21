import decipher
import gleam/dynamic
import gleam/json
import gleam/option.{Some}

pub const query = "
query getStargazers($name: String!, $owner: String!) {
  repository(owner: $owner, name: $name) {
    stargazerCount
  }
}"

pub fn decoder(dyn) {
  decipher.at(["data", "repository", "stargazerCount"], dynamic.int)(dyn)
}

pub fn variables(name: String, owner: String) {
  Some(
    json.object([#("name", json.string(name)), #("owner", json.string(owner))]),
  )
}
