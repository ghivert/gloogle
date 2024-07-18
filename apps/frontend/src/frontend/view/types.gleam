import frontend/colors/palette
import lustre/attribute as a
import lustre/element/html as h

fn span(text: String, color: String) {
  h.span([a.style([#("color", color)])], [h.text(text)])
}

pub fn keyword(text: String) {
  span(text, "var(--hljs-hue-3)")
}

pub fn fun(text: String) {
  span(text, "var(--hljs-hue-2)")
}

pub fn label(text: String) {
  span(text, "var(--hljs-hue-5)")
}

pub fn type_(text: String) {
  span(text, "var(--hljs-hue-6-2)")
}

pub fn variable(text: String) {
  span(text, "var(--hljs-hue-4)")
}

pub fn dark_white(text: String) {
  span(text, palette.dark.dark_white)
}

pub fn white(text: String) {
  span(text, "var(--text-color)")
}
