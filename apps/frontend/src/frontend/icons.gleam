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

pub fn external_link() {
  let content =
    "<path fill-rule=\"evenodd\" clip-rule=\"evenodd\" d=\"M4.25 5.5C3.83579 5.5 3.5 5.83579 3.5 6.25L3.5 14.75C3.5 15.1642 3.83579 15.5 4.25 15.5L12.75 15.5C13.1642 15.5 13.5 15.1642 13.5 14.75V10.75C13.5 10.3358 13.8358 10 14.25 10C14.6642 10 15 10.3358 15 10.75V14.75C15 15.9926 13.9926 17 12.75 17H4.25C3.00736 17 2 15.9926 2 14.75L2 6.25C2 5.00736 3.00736 4 4.25 4L9.25 4C9.66421 4 10 4.33579 10 4.75C10 5.16421 9.66421 5.5 9.25 5.5L4.25 5.5Z\" fill=\"white\"/>
    <path fill-rule=\"evenodd\" clip-rule=\"evenodd\" d=\"M6.19385 12.7532C6.47175 13.0603 6.94603 13.0841 7.25319 12.8062L16.5 4.43999V7.25C16.5 7.66421 16.8358 8 17.25 8C17.6642 8 18 7.66421 18 7.25V2.75C18 2.33579 17.6642 2 17.25 2L12.75 2C12.3358 2 12 2.33579 12 2.75C12 3.16421 12.3358 3.5 12.75 3.5L15.3032 3.5L6.24682 11.6938C5.93966 11.9717 5.91595 12.446 6.19385 12.7532Z\" fill=\"white\"/>"
  html.svg(
    [
      attribute.attribute("dangerous-unescaped-html", content),
      attribute.style([#("width", "100%"), #("height", "100%")]),
      attribute.attribute("viewBox", "0 0 20 20"),
      attribute.attribute("xmlns", "http://www.w3.org/2000/svg"),
      attribute.attribute("fill", "currentColor"),
    ],
    [],
  )
}

pub fn arrow() {
  let content =
    "<path fill-rule=\"evenodd\" d=\"M12.53 16.28a.75.75 0 0 1-1.06 0l-7.5-7.5a.75.75 0 0 1 1.06-1.06L12 14.69l6.97-6.97a.75.75 0 1 1 1.06 1.06l-7.5 7.5Z\" clip-rule=\"evenodd\"></path>"
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
