import sketch as s

pub fn paragraph() {
  s.class([s.white_space("pre-wrap")])
  |> s.memo()
  |> s.to_lustre()
}
