import api/github/stargazer_count
import gleam/dynamic/decode.{type Decoder}
import gleam/function
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, Some}
import gleam/regexp
import gleam/result
import jupiter/error

pub fn get_stargazer_count(token: String, repo_url: String) {
  use match <- result.try(match_repository_name(repo_url))
  case match.submatches {
    [Some(owner), Some(name)] -> {
      let variables = stargazer_count.variables(name, owner)
      let returning = stargazer_count.decoder()
      query(token:, query: stargazer_count.query, variables:, returning:)
    }
    _ -> Error(error.UnknownError(""))
  }
}

fn query(
  token token: String,
  query query: String,
  variables variables: Option(Json),
  returning decoder: Decoder(a),
) -> Result(a, error.Error) {
  use response <- result.try({
    request.new()
    |> request.set_header("authorization", "Bearer " <> token)
    |> request.set_header("user-agent", "gloogle / 1.0.0")
    |> request.set_method(http.Post)
    |> request.set_scheme(http.Https)
    |> request.set_host("api.github.com")
    |> request.set_path("/graphql")
    |> request.set_body(encode_body(query, variables))
    |> httpc.send
    |> result.map_error(error.HttpcError)
  })

  response.body
  |> json.parse(using: decoder)
  |> result.map_error(error.JsonError)
}

fn encode_body(query: String, variables: Option(Json)) -> String {
  json.to_string({
    json.object([
      #("query", json.string(query)),
      #("variables", json.nullable(variables, function.identity)),
    ])
  })
}

fn match_repository_name(repo_url: String) {
  let assert Ok(owner_name) = regexp.from_string("https://github.com/(.+)/(.+)")
  let err = "No repository match for " <> repo_url
  regexp.scan(with: owner_name, content: repo_url)
  |> list.first
  |> result.replace_error(error.UnknownError(err))
}
