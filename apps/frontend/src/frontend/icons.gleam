import lustre/attribute
import lustre/element/html

pub fn trends() {
  let content =
    "<path fill-rule=\"evenodd\" d=\"M15.22 6.268a.75.75 0 0 1 .968-.431l5.942 2.28a.75.75 0 0 1 .431.97l-2.28 5.94a.75.75 0 1 1-1.4-.537l1.63-4.251-1.086.484a11.2 11.2 0 0 0-5.45 5.173.75.75 0 0 1-1.199.19L9 12.312l-6.22 6.22a.75.75 0 0 1-1.06-1.061l6.75-6.75a.75.75 0 0 1 1.06 0l3.606 3.606a12.695 12.695 0 0 1 5.68-4.974l1.086-.483-4.251-1.632a.75.75 0 0 1-.432-.97Z\" clip-rule=\"evenodd\"></path>"
  html.svg(
    [
      attribute.attribute("dangerous-unescaped-html", content),
      attribute.style([#("width", "100%"), #("height", "100%")]),
      attribute.attribute("viewBox", "0 0 24 24"),
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("fill", "currentColor"),
    ],
    [],
  )
}

pub fn shortcuts() {
  let content =
    "<path fill-rule=\"evenodd\" d=\"M14.447 3.026a.75.75 0 0 1 .527.921l-4.5 16.5a.75.75 0 0 1-1.448-.394l4.5-16.5a.75.75 0 0 1 .921-.527ZM16.72 6.22a.75.75 0 0 1 1.06 0l5.25 5.25a.75.75 0 0 1 0 1.06l-5.25 5.25a.75.75 0 1 1-1.06-1.06L21.44 12l-4.72-4.72a.75.75 0 0 1 0-1.06Zm-9.44 0a.75.75 0 0 1 0 1.06L2.56 12l4.72 4.72a.75.75 0 0 1-1.06 1.06L.97 12.53a.75.75 0 0 1 0-1.06l5.25-5.25a.75.75 0 0 1 1.06 0Z\" clip-rule=\"evenodd\"></path>"
  html.svg(
    [
      attribute.attribute("dangerous-unescaped-html", content),
      attribute.style([#("width", "100%"), #("height", "100%")]),
      attribute.attribute("viewBox", "0 0 24 24"),
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("fill", "currentColor"),
    ],
    [],
  )
}

pub fn gift() {
  let content =
    "<path d=\"M9.375 3a1.875 1.875 0 0 0 0 3.75h1.875v4.5H3.375A1.875 1.875 0 0 1 1.5 9.375v-.75c0-1.036.84-1.875 1.875-1.875h3.193A3.375 3.375 0 0 1 12 2.753a3.375 3.375 0 0 1 5.432 3.997h3.943c1.035 0 1.875.84 1.875 1.875v.75c0 1.036-.84 1.875-1.875 1.875H12.75v-4.5h1.875a1.875 1.875 0 1 0-1.875-1.875V6.75h-1.5V4.875C11.25 3.839 10.41 3 9.375 3ZM11.25 12.75H3v6.75a2.25 2.25 0 0 0 2.25 2.25h6v-9ZM12.75 12.75v9h6.75a2.25 2.25 0 0 0 2.25-2.25v-6.75h-9Z\"></path>"
  html.svg(
    [
      attribute.attribute("dangerous-unescaped-html", content),
      attribute.style([#("width", "100%"), #("height", "100%")]),
      attribute.attribute("viewBox", "0 0 24 24"),
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("fill", "currentColor"),
    ],
    [],
  )
}

pub fn heart() {
  let content =
    "<path d=\"m11.645 20.91-.007-.003-.022-.012a15.247 15.247 0 0 1-.383-.218 25.18 25.18 0 0 1-4.244-3.17C4.688 15.36 2.25 12.174 2.25 8.25 2.25 5.322 4.714 3 7.688 3A5.5 5.5 0 0 1 12 5.052 5.5 5.5 0 0 1 16.313 3c2.973 0 5.437 2.322 5.437 5.25 0 3.925-2.438 7.111-4.739 9.256a25.175 25.175 0 0 1-4.244 3.17 15.247 15.247 0 0 1-.383.219l-.022.012-.007.004-.003.001a.752.752 0 0 1-.704 0l-.003-.001Z\"></path>"
  html.svg(
    [
      attribute.attribute("dangerous-unescaped-html", content),
      attribute.style([#("width", "100%"), #("height", "100%")]),
      attribute.attribute("viewBox", "0 0 24 24"),
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("fill", "currentColor"),
    ],
    [],
  )
}
