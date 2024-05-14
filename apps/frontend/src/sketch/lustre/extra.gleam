import gleam/list
import lustre/attribute.{type Attribute}
import lustre/element.{type Element}
import sketch as s

pub fn element(
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
  styles: List(s.Style(media, pseudo_selector)),
) {
  s.class(styles)
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element(tag, _, children)
}

pub fn memo(
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
  styles: List(s.Style(media, pseudo_selector)),
) {
  s.class(styles)
  |> s.memo()
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element(tag, _, children)
}

pub fn dynamic(
  tag: String,
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
  id: String,
  styles: List(s.Style(media, pseudo_selector)),
) {
  s.dynamic(id, styles)
  |> s.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element(tag, _, children)
}
