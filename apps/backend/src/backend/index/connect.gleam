import backend/config.{type Config, type Context, Context}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/pgo.{Config}
import gleam/regex
import gleam/result
import wisp.{type Response}

pub fn connect(cnf: Config) {
  let assert Ok(config) = parse_database_url(cnf.database_url)
  config
  |> pgo.connect()
  |> Context
}

pub fn middleware(cnf: Config, handler: fn(Context) -> Response) {
  connect(cnf)
  |> handler()
}

fn database_url_matcher() {
  "postgres://(.*):(.*)@(.*):(.*)/(.*)\\?sslmode=(.*)"
  |> regex.from_string()
  |> result.replace_error(Nil)
}

fn parse_database_url(database_url: String) -> Result(pgo.Config, _) {
  use matcher <- result.map(database_url_matcher())
  let matches = regex.scan(with: matcher, content: database_url)
  use cnf, data <- list.fold(matches, pgo.default_config())
  use acc, m, index <- list.index_fold(data.submatches, cnf)
  case index, m {
    0, Some(v) -> Config(..acc, user: v)
    1, Some(v) -> Config(..acc, password: Some(v))
    2, Some(v) -> Config(..acc, host: v)
    3, Some(v) -> Config(..acc, port: result.unwrap(int.parse(v), 5432))
    4, Some(v) -> Config(..acc, database: v)
    5, Some(v) -> Config(..acc, ssl: v == "true")
    _, _ -> acc
  }
}
