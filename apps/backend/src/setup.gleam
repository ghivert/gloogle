import backend/config
import radiate
import wisp

fn print_radiate_update(_state: state, path: String) {
  wisp.log_debug("Change in " <> path <> ", reloading")
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
      wisp.log_debug("Watching src to change.")
    }
  }
}
