# Usage with BEAM

Sketch can be transparently used with BEAM and JS, however, due to the runtime,
some differences exists, and should be taken in consideration. Contrarily to JS,
BEAM is a runtime which is actor oriented, and synchronous.

To generate the real stylesheet the application will use, sketch uses an
intermediate virtual stylesheet, called "cache". In BEAM, each process needs to
create cache.

Just like with any app, creating a cache relies on
[`create_cache`](https://hexdocs.pm/sketch/sketch.html#create_cache). Once
created, a cache will live as long as you keep a reference on it. The strategy
is to prepare and render the cache before and after every HTML generation. You
can use [`prepare`](https://hexdocs.pm/sketch/sketch.html#prepare) and
[`render`](https://hexdocs.pm/sketch/sketch.html#render) respectively before and
after HTML generation. `prepare` will setup the cache as being the default cache
for the current process, and `render` will turn the cache into a proper
stylesheet, i.e. it will returns a `String`, containing the stylesheet content.
All you have to do then is to embed the resulting string in your output HTML,
and voilà!

## Example

The example assumes you're using a blank, empty
[`mist`](https://hexdocs.pm/mist) server. It could feel a little cumbersome, but
most of the time, your framework should handle it directly.

```gleam
import gleam/bytes_builder
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/http/response.{type Response}
import gleam/string
import mist.{type Connection, type ResponseData}
import sketch
import sketch/options as sketch_options

pub fn main() {
  // Creates the cache here. A global cache will be used here, because the SSR
  // only uses one process.
  let assert Ok(cache) =
    sketch_options.node()
    |> sketch.create_cache()

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

// Defines a class, here with simple class name.
fn main_class() {
  [sketch.color("red")]
  |> sketch.class()
  |> sketch.to_class_name()
}

fn greet(name: String, cache: sketch.Cache) -> Response(ResponseData) {
  let res = response.new(200)
  let greetings = "<div>Hey there, " <> name <> "!</div>"

  // Create your body, with the classes computed.
  let body =
    [
      "<div class=" <> main_class() <> ">",
      "  <div>Hello World!</div>",
      greetings,
      "</div>",
    ]
    |> string.join("\n")

  // Get the content of your stylesheet.
  let assert Ok(stylesheet_content) = sketch.render(cache)

  // Render the style in your HTML.
  let html =
    [
      "<html>",
      "  <head>",
      "    <style>",
      stylesheet_content,
      "    </style>",
      "  </head>",
      "  <body>",
      body,
      "  </body>",
      "</html>",
    ]
    |> string.join("\n")

  html
  |> bytes_builder.from_string()
  |> mist.Bytes()
  |> response.set_body(res, _)
}
```

## More details

You can find more details on how it works in the
[internal details](https://hexdocs.pm/sketch/internal-details.html).
