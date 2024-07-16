import gleam/bytes_builder
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import lustre/element
import lustre/element/html.{html}
import mist.{type Connection, type ResponseData}
import sketch
import sketch/lustre as sketch_lustre
import sketch/options as sketch_options

pub fn main() {
  let assert Ok(cache) =
    sketch_options.node()
    |> sketch_lustre.setup()

  let empty_body = mist.Bytes(bytes_builder.new())
  let not_found = response.set_body(response.new(404), empty_body)

  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      sketch.prepare(cache)
      case request.path_segments(req) {
        ["greet", name] -> greet(name, cache)
        _ -> not_found
      }
    }
    |> mist.new()
    |> mist.port(3000)
    |> mist.start_http()

  process.sleep_forever()
}

fn main_class() {
  [sketch.color("red")]
  |> sketch.class()
  |> sketch.to_lustre()
}

fn greet(name: String, cache: sketch.Cache) -> Response(ResponseData) {
  let res = response.new(200)
  let greetings = "Hey there, " <> name <> "!"
  let html =
    html([], [
      html.head([], [html.title([], "Greetings!")]),
      html.body([], [html.h1([main_class()], [html.text(greetings)])]),
    ])

  html
  |> sketch_lustre.ssr(cache)
  |> element.to_document_string()
  |> bytes_builder.from_string()
  |> mist.Bytes()
  |> response.set_body(res, _)
}
