import gleam/erlang/os
import gleam/io
import radiate
import wisp

pub fn is_dev() {
  os.get_env("GLEAM_ENV") == Ok("development")
}

pub fn get_secret_key_base() {
  wisp.random_string(64)
}

fn print_radiate_update(_state: state, path: String) {
  io.println("Change in " <> path <> ", reloading")
}

pub fn radiate() {
  case is_dev() {
    False -> Nil
    True -> {
      let assert Ok(_) =
        radiate.new()
        |> radiate.add_dir(".")
        |> radiate.on_reload(print_radiate_update)
        |> radiate.start()
      io.println("Watching src to change.")
    }
  }
}
