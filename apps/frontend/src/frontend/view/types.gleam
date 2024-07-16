import frontend/colors/palette
import lustre/element/html as h
import sketch as s
import sketch/lustre/element

fn span(text: String, color: String) {
  element.element("span", [], [element.text(text)], [s.color(color)])
}

pub fn keyword(text: String) {
  span(text, "#c678dd")
}

pub fn fun(text: String) {
  span(text, "#61afef")
}

pub fn label(text: String) {
  span(text, "#e06c75")
}

pub fn type_(text: String) {
  span(text, "#e5c07b")
}

pub fn variable(text: String) {
  span(text, "#98c379")
}

pub fn dark_white(text: String) {
  span(text, palette.dark.dark_white)
}
