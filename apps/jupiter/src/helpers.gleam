import gleam/dict.{type Dict}
import gleam/json
import gleam/list
import gleam/pair

pub fn json_dict(dict: Dict(String, String)) {
  dict
  |> dict.to_list
  |> list.map(pair.map_second(_, json.string))
  |> json.object
  |> json.to_string
}

pub fn json_list(list: List(String)) {
  list
  |> json.array(of: json.string)
  |> json.to_string()
}
