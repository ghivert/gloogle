import gleam/dynamic

pub type Implementations {
  Implementations(
    gleam: Bool,
    uses_erlang_externals: Bool,
    uses_javascript_externals: Bool,
  )
}

pub fn decode_implementations(dyn) {
  dynamic.decode3(
    Implementations,
    dynamic.field("gleam", dynamic.bool),
    dynamic.field("uses_erlang_externals", dynamic.bool),
    dynamic.field("uses_javascript_externals", dynamic.bool),
  )(dyn)
}
