import gleam/list

pub fn postpend(list: List(a), value: a) {
  list.append(list, [value])
}
