import gleam/dynamic.{type Dynamic}
import gleam/json
import gleam/option
import gleam/result

pub fn completely_option(field: String) {
  fn(dyn: Dynamic) {
    dynamic.optional_field(field, dynamic.optional(dynamic.string))(dyn)
    |> result.map(fn(res) { option.flatten(res) })
  }
}

pub fn json(decoder: dynamic.Decoder(a)) {
  dynamic.any([
    decoder,
    fn(dyn) {
      case dynamic.string(dyn) {
        Error(e) -> Error(e)
        Ok(content) ->
          json.decode(content, decoder)
          |> result.replace_error([])
      }
    },
  ])
}
