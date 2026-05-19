import gleam/dynamic/decode
import gleam/json
import gleam/option.{Some}

pub const query = "
query getStargazers($name: String!, $owner: String!) {
  repository(owner: $owner, name: $name) {
    stargazerCount
  }
}"

pub fn decoder() {
  decode.at(["data", "repository", "stargazerCount"], decode.int)
}

pub fn variables(name: String, owner: String) {
  let name = json.string(name)
  let owner = json.string(owner)
  Some(json.object([#("name", name), #("owner", owner)]))
}
