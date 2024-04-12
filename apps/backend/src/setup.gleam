import backend/config
import gleam/io
import radiate

fn print_radiate_update(_state: state, path: String) {
  io.println("Change in " <> path <> ", reloading")
}

pub fn radiate() {
  case config.is_dev() {
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
