import birl.{type Time}
import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/pgo
import gleam/result

pub fn convert_time(time: Time) {
  time
  |> birl.to_iso8601()
  |> pgo.text()
}

pub fn decode_time(data: Dynamic) {
  data
  |> dynamic.tuple2(decode_time_tuple, decode_time_tuple)
  |> result.map(birl.from_erlang_universal_datetime)
}

fn decode_time_tuple(data: Dynamic) {
  dynamic.tuple3(
    dynamic.int,
    dynamic.int,
    dynamic.any([dynamic.int, rounded_float]),
  )(data)
}

fn rounded_float(data: Dynamic) {
  data
  |> dynamic.float()
  |> result.map(float.round)
}
