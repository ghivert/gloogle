import api/github/stargazer_count
import backend/error
import gleam/dynamic
import gleam/function
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/option.{type Option, Some}
import gleam/regex
import gleam/result

fn query(
  token: String,
  query: String,
  variables: Option(json.Json),
  decoder: dynamic.Decoder(a),
) {
  let body =
    json.object([
      #("query", json.string(query)),
      #("variables", json.nullable(variables, function.identity)),
    ])
  use response <- result.try(
    request.new()
    |> request.set_header("authorization", "Bearer " <> token)
    |> request.set_header("user-agent", "gloogle / 0.0.0")
    |> request.set_method(http.Post)
    |> request.set_scheme(http.Https)
    |> request.set_host("api.github.com")
    |> request.set_path("/graphql")
    |> request.set_body(json.to_string(body))
    |> httpc.send()
    |> result.map_error(error.FetchError),
  )

  response.body
  |> json.decode(using: decoder)
  |> result.map_error(error.JsonError)
}

fn match_repository_name(repo_url: String) {
  let assert Ok(owner_name) = regex.from_string("https://github.com/(.+)/(.+)")
  regex.scan(with: owner_name, content: repo_url)
  |> list.first()
  |> result.replace_error(error.UnknownError(
    "No repository match for " <> repo_url,
  ))
}

pub fn get_stargazer_count(token: String, repo_url: String) {
  use match <- result.try(match_repository_name(repo_url))
  case match.submatches {
    [Some(owner), Some(name)] ->
      stargazer_count.variables(name, owner)
      |> query(token, stargazer_count.query, _, stargazer_count.decoder)
    _ -> Error(error.UnknownError(""))
  }
}
