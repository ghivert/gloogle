import gleam/erlang/charlist.{type Charlist}

pub fn cmd(command: String) -> String {
  command
  |> charlist.from_string
  |> do_cmd
  |> charlist.to_string
}

@external(erlang, "os", "cmd")
fn do_cmd(command: Charlist) -> Charlist
