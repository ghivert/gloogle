# Usage with Lustre SSR

To use sketch with lustre SSR, little modifications are necessary. Sketch
bundles helpers to work with lustre SSR right out-of-the-box. Just use them, and
you'll be good to go. Like every lustre application, it has to be setuped with
[`setup`](https://hexdocs.pm/sketch/sketch/lustre.html#setup), but after that,
you'll have to use [`prepare`](https://hexdocs.pm/sketch/sketch.html#prepare) in
your HTTP handler, and at the end of the HTML generation, when you still have an
[`Element`](https://hexdocs.pm/lustre/lustre/element.html#Element) from lustre,
run [`ssr`](https://hexdocs.pm/sketch/sketch/lustre.html#ssr) and let the magic
happen!

## Example

An example with lustre, and with SSR. The idea here is to instanciate a cache,
and to reuses it for every request. To scale it properly, the idea would be to
create a cache for every process started.

```gleam
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
  // Creates the cache here. A global cache will be used here, because the SSR
  // only uses one process.
  let assert Ok(cache) =
    sketch_options.node()
    |> sketch_lustre.setup()

  // Helpers for mist server.
  let empty_body = mist.Bytes(bytes_builder.new())
  let not_found = response.set_body(response.new(404), empty_body)

  let assert Ok(_) =
    fn(req: Request(Connection)) -> Response(ResponseData) {
      // Before every request, prepare the cache, to wipe it from old data, and
      // mark it as "default" cache for the following render.
      sketch.prepare(cache)
      case request.path_segments(req) {
        // Send the cache to your render function. It will be used before
        // returing the HTML.
        ["greet", name] -> greet(name, cache)
        _ -> not_found
      }
    }
    |> mist.new()
    |> mist.port(3000)
    |> mist.start_http()

  process.sleep_forever()
}

// Defines a class, here with lustre SSR.
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
      // Use the class as usual.
      html.body([], [html.h1([main_class()], [html.text(greetings)])]),
    ])

  html
  // Ask for sketch to inject the cache content in the resulting HTML from lustre.
  |> sketch_lustre.ssr(cache)
  |> element.to_document_string()
  |> bytes_builder.from_string()
  |> mist.Bytes()
  |> response.set_body(res, _)
}
```

## Feedbacks

Because we lack people really using SSR, we take all feedbacks to help us
improve the API for SSR with sketch! Feel free to open an issue to discuss about
the API!
