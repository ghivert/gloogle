import backend/config.{type Config}
import backend/router
import dot_env
import gleam/erlang/process
import gleam/otp/supervisor
import mist
import periodic
import setup
import wisp
import tasks/hex

pub fn main() {
  setup.radiate()
  wisp.configure_logger()
  dot_env.load()

  let secret_key_base = config.get_secret_key_base()
  let cnf = config.read_config()

  let assert Ok(_) =
    router.handle_request(_, cnf)
    |> wisp.mist_handler(secret_key_base)
    |> mist.new()
    |> mist.port(3000)
    |> mist.start_http()

  let _ = start_hex_sync(cnf)

  process.sleep_forever()
}

fn supervise(start: fn() -> _) {
  use children <- supervisor.start()
  supervisor.add(children, {
    use _ <- supervisor.worker()
    start()
  })
}

fn sync_hex(cnf: Config) {
  hex.sync_new_gleam_releases(cnf)
}

fn start_hex_sync(cnf: Config) {
  use <- supervise()
  periodic.periodically(do: fn() { sync_hex(cnf) }, waiting: 60 * 1000)
}
