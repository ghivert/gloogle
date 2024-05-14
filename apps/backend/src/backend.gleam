import backend/config.{type Config}
import backend/postgres/postgres
import backend/router
import dot_env
import gleam/erlang/process
import gleam/otp/supervisor
import mist
import periodic
import setup
import tasks/hex
import wisp
import wisp/logger

pub fn main() {
  dot_env.load()
  setup.radiate()
  wisp.configure_logger()
  logger.set_level(logger.Debug)

  let secret_key_base = config.get_secret_key_base()
  let cnf = config.read_config()
  let ctx = postgres.connect(cnf)

  let assert Ok(_) =
    router.handle_request(_, ctx)
    |> wisp.mist_handler(secret_key_base)
    |> mist.new()
    |> mist.port(cnf.port)
    |> mist.start_http()

  let assert Ok(_) =
    supervisor.start(fn(children) {
      let assert Ok(_) = start_hex_sync(cnf, children)
      children
    })

  process.sleep_forever()
}

fn supervise(start: fn() -> _) {
  use children <- supervisor.start()
  supervisor.add(children, {
    use _ <- supervisor.worker()
    start()
  })
}

fn sync_hex(cnf: Config, children: supervisor.Children(Nil)) {
  hex.sync_new_gleam_releases(cnf, children)
}

fn start_hex_sync(cnf: Config, children: supervisor.Children(Nil)) {
  use <- supervise()
  periodic.periodically(
    do: fn() { sync_hex(cnf, children) },
    waiting: 60 * 1000,
  )
}
