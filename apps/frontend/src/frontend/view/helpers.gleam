import gleam/string
import lustre/element/html as h

pub fn newline() {
  h.text("\n")
}

pub fn idt(indent: Int) {
  h.text(string.repeat(" ", indent))
}
