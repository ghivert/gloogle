import backend/error
import gleam/bool
import gleam/list
import gleam/option.{type Option, Some}
import gleam/result
import gleam/string
import gleam/uri
import pog.{Config}

pub fn connect(database_url: String) {
  let assert Ok(config) = parse_database_url(database_url)
  config
  |> pog.rows_as_map(True)
  |> pog.connect
}

fn parse_database_url(database_url: String) {
  use db_uri <- result.try({
    uri.parse(database_url)
    |> result.replace_error(error.UnknownError("Unable to parse database URL"))
  })
  use <- bool.guard(
    when: db_uri.scheme != Some("postgres"),
    return: Error(error.UnknownError("No postgres protocol")),
  )

  pog.default_config()
  |> fn(cnf) { Config(..cnf, database: string.replace(db_uri.path, "/", "")) }
  |> update_config(db_uri.userinfo, add_user_info)
  |> update_config(db_uri.host, fn(cnf, u) { Config(..cnf, host: u) })
  |> update_config(db_uri.port, fn(cnf, p) { Config(..cnf, port: p) })
  |> update_config(db_uri.query, fn(cnf, q) {
    uri.parse_query(q)
    |> result.then(list.key_find(_, "sslmode"))
    |> result.map(fn(ssl) {
      let is_ssl_enabled = ssl != "disable"
      Config(..cnf, ssl: is_ssl_enabled)
    })
    |> result.unwrap(cnf)
  })
  |> Ok
}

fn update_config(
  cnf: pog.Config,
  field: Option(a),
  mapper: fn(pog.Config, a) -> pog.Config,
) {
  option.map(field, fn(u) { mapper(cnf, u) })
  |> option.unwrap(cnf)
}

fn add_user_info(c: pog.Config, u: String) {
  case string.split(u, ":") {
    [user, password] -> Config(..c, user: user, password: Some(password))
    [user] -> Config(..c, user: user)
    _ -> c
  }
}
