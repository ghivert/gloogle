import birl.{type Time}
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/float
import gleam/json
import gleam/list
import gleam/pair
import gleam/pgo
import gleam/result

pub fn convert_time(time: Time) -> pgo.Value {
  time
  |> birl.to_erlang_universal_datetime()
  |> dynamic.from()
  |> dynamic.unsafe_coerce()
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

pub fn json_dict(dict: Dict(String, String)) {
  dict
  |> dict.to_list()
  |> list.map(fn(t) { pair.map_second(t, json.string) })
  |> json.object()
  |> json.to_string()
}

pub fn json_list(list: List(String)) {
  list
  |> json.array(of: json.string)
  |> json.to_string()
}
