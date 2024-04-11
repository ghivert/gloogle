import backend/router
import gleam/erlang/process
import mist
import setup
import wisp

pub fn main() {
  setup.radiate()
  wisp.configure_logger()

  let secret_key_base = setup.get_secret_key_base()

  let assert Ok(_) =
    wisp.mist_handler(router.handle_request, secret_key_base)
    |> mist.new()
    |> mist.port(3000)
    |> mist.start_http()

  process.sleep_forever()
}
