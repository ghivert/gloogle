import lustre/attribute as a
import sketch/lustre/element/html

const content = "<rect width=\"256\" height=\"256\" fill=\"none\"/><polyline points=\"168 168 216 168 216 40 88 40 88 88\" fill=\"none\" stroke=\"currentColor\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"16\"/><rect x=\"40\" y=\"88\" width=\"128\" height=\"128\" fill=\"none\" stroke=\"currentColor\" stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"16\"/>"

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
