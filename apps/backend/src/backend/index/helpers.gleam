import birl.{type Time}
import gleam/dynamic.{type Dynamic}
import gleam/pgo

pub fn convert_time(time: Time) {
  time
  |> birl.to_iso8601()
  |> pgo.text()
}

pub fn decode_time(dynamic: Dynamic) {
  case dynamic.string(dynamic) {
    Error(e) -> Error(e)
    Ok(value) ->
      case birl.parse(value) {
        Error(_) -> Error([dynamic.DecodeError("", "", [])])
        Ok(value) -> Ok(value)
      }
  }
}
