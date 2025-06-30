import gleam/dynamic/decode

pub type Implementations {
  Implementations(
    gleam: Bool,
    uses_erlang_externals: Bool,
    uses_javascript_externals: Bool,
  )
}

pub fn decoder() {
  use gleam <- decode.field("gleam", decode.bool)
  use erlang <- decode.field("uses_erlang_externals", decode.bool)
  use js <- decode.field("uses_javascript_externals", decode.bool)
  decode.success({
    Implementations(
      gleam:,
      uses_erlang_externals: erlang,
      uses_javascript_externals: js,
    )
  })
}
