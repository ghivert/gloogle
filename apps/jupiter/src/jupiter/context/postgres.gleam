import gleam/erlang/atom
import gleam/erlang/process
import gleam/list
import gleam/option.{Some}
import gleam/result
import gleam/string
import gleam/unsafe
import gleam/uri
import jupiter/context/environment
import jupiter/context/environment/variables
import palabres
import pog

const module = "jupiter/context/postgres"

pub fn supervised() {
  let database_url = variables.database_url()
  let ssl = ssl(database_url, environment.read())
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

/// TLS mode of the connection. An explicit `sslmode` in `DATABASE_URL` wins,
/// with libpq's meaning — `disable`, `require` (encrypted, unverified),
/// `verify-ca` / `verify-full` (pog checks the CA and the hostname for both).
/// Without one: no TLS in development, verified TLS in production.
///
/// Production on Clever Cloud goes through the network group with
/// `sslmode=require`: the add-on's certificate is self-signed per instance,
/// which Erlang's `ssl` refuses as a peer even when trusted (`selfsigned_peer`),
/// and WireGuard already authenticates both ends.
pub fn ssl(database_url: String, env: environment.Environment) -> pog.Ssl {
  let sslmode = {
    use parsed <- result.try(uri.parse(database_url))
    use query <- result.try(case parsed.query {
      Some(query) -> Ok(query)
      _ -> Error(Nil)
    })
    use params <- result.try(uri.parse_query(query))
    list.key_find(params, "sslmode")
  }
  case sslmode, env {
    Ok("disable"), _ -> pog.SslDisabled
    Ok("require"), _ -> pog.SslUnverified
    Ok("verify-ca"), _ | Ok("verify-full"), _ -> pog.SslVerified
    _, environment.Development -> pog.SslDisabled
    _, environment.Production -> pog.SslVerified
  }
}
