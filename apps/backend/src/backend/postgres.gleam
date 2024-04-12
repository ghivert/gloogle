import backend/config.{type Config, type Context, Context}
import gleam/dynamic
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
  "postgres://(.*):(.*)@(.*):(.*)/(.*)"
  |> regex.from_string()
  |> result.replace_error(Nil)
}

fn parse_database_url(database_url: String) -> Result(pgo.Config, _) {
  use matcher <- result.map(database_url_matcher())
  let matches = regex.scan(with: matcher, content: database_url)
  use cnf, data, index <- list.index_fold(matches, pgo.default_config())
  case index {
    0 -> Config(..cnf, user: data.content)
    1 -> Config(..cnf, password: Some(data.content))
    2 -> Config(..cnf, host: data.content)
    3 -> Config(..cnf, port: result.unwrap(int.parse(data.content), 5432))
    4 -> Config(..cnf, database: data.content)
    _ -> cnf
  }
}
