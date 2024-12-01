import lustre/attribute as a
import sketch/lustre/element/html

const content = "<rect width=\"256\" height=\"256\" fill=\"none\"/><polyline points=\"40 144 96 200 224 72\" fill=\"none\" stroke=\"currentColor\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"16\"/>"

pub fn icon() {
  html.svg_(
    [
      a.style([#("max-width", "100%"), #("max-height", "100%")]),
      a.attribute("xmlns", "http://www.w3.org/2000/svg"),
      a.attribute("viewBox", "0 0 256 256"),
      a.attribute("fill", "currentColor"),
      a.attribute("dangerous-unescaped-html", content),
    ],
    [],
  )
}
