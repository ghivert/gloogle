import gleam/list
import lustre/element/html as h
import sketch as s

pub fn keyword(text: String) {
  s.class([s.color("#c678dd")])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend([], _)
  |> h.span([h.text(text)])
}

pub fn fun(text: String) {
  s.class([s.color("#61afef")])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend([], _)
  |> h.span([h.text(text)])
}

pub fn label(text: String) {
  s.class([s.color("#e06c75")])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend([], _)
  |> h.span([h.text(text)])
}

pub fn type_(text: String) {
  s.class([s.color("#e5c07b")])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend([], _)
  |> h.span([h.text(text)])
}

pub fn variable(text: String) {
  s.class([s.color("#98c379")])
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend([], _)
  |> h.span([h.text(text)])
}
