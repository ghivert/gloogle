import lustre/attribute.{type Attribute}
import lustre/element as el
import sketch
import sketch/magic

pub const keyed = el.keyed

pub const fragment = el.fragment

pub const none = el.none

pub const text = el.text

pub const map = el.map

pub fn element(
  tag tag: String,
  class class: sketch.Class,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  let class_name = magic.class_name(class)
  el.element(tag, [attribute.class(class_name), ..attributes], children)
}

pub fn element_(
  tag tag: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  el.element(tag, attributes, children)
}

pub fn namespaced(
  tag tag: String,
  namespace namespace: String,
  class class: sketch.Class,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  let class_name = magic.class_name(class)
  let attributes = [attribute.class(class_name), ..attributes]
  el.namespaced(tag, namespace, attributes, children)
}

pub fn namespaced_(
  tag tag: String,
  namespace namespace: String,
  attributes attributes: List(Attribute(msg)),
  children children: List(el.Element(msg)),
) {
  el.namespaced(tag, namespace, attributes, children)
}
