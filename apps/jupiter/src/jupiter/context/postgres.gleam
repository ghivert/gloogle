import gleam/erlang/atom
import gleam/erlang/process
import gleam/result
import gleam/string
import gleam/unsafe
import jupiter/context/environment
import jupiter/context/environment/variables
import palabres
import pog

const module = "jupiter/context/postgres"

pub fn supervised() {
  let database_url = variables.database_url()
  let ssl = postgres_ssl()
  palabres.notice("Supervision: starting.")
  |> palabres.string("ssl", string.inspect(ssl))
  |> palabres.at(module, "supervised")
  |> palabres.log

  let connection_name = name()
  let config = pog.url_config(connection_name, database_url)
  use config <- result.map(config)
  palabres.notice("Supervision: OK.")
  |> palabres.string("ssl", string.inspect(ssl))
  |> palabres.at(module, "supervised")
  |> palabres.log

  config
  |> pog.ssl(ssl)
  |> pog.rows_as_map(True)
  |> pog.supervised
}

pub fn connection() {
  let connection_name = name()
  pog.named_connection(connection_name)
}

pub fn name() -> process.Name(pog.Message) {
  atom.create("jupiter_pog")
  |> unsafe.coerce
}

fn postgres_ssl() {
  case environment.is_dev() {
    True -> pog.SslDisabled
    False -> pog.SslVerified
  }
}
