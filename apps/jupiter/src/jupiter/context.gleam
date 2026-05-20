import cell.{type Cell}
import envoy
import gleam/option.{type Option, None}
import gleam/result
import jupiter/context/environment.{type Environment}
import jupiter/context/postgres
import jupiter/gleam/type_search/search
import jupiter/web
import pog
import wisp.{type Request, type Response}

pub type Context {
  Context(
    db: pog.Connection,
    hex_api_key: String,
    github_token: String,
    env: Environment,
    table: cell.Table,
    search: Cell(search.TypeSearch),
    trace_id: Option(String),
  )
}

pub fn init() {
  let env = environment.read()
  use hex_api_key <- result.try(envoy.get("HEX_API_KEY"))
  use github_token <- result.map(envoy.get("GITHUB_TOKEN"))
  let db = postgres.connection()
  let table = cell.new_table()
  let search = cell.new(table)
  let trace_id = None
  Context(db:, hex_api_key:, github_token:, env:, table:, search:, trace_id:)
}

pub fn get_secret_key_base() {
  wisp.random_string(64)
}

pub fn set_trace_id(
  req: Request,
  ctx: Context,
  next: fn(Context) -> Response,
) -> Response {
  use trace_id <- web.trace_id(req)
  Context(..ctx, trace_id:)
  |> next
}
