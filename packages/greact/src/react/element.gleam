pub fn div(attrs, children: List(Component)) -> Component {
  jsx("div", attrs, children)
}

pub fn a(attrs, children: List(Component)) -> Component {
  jsx("a", attrs, children)
}

pub fn img(attrs) -> Component {
  jsx("img", attrs, [])
}

pub fn text(content) -> Component {
  jsx("text_", Nil, content)
}
