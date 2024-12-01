import lustre/attribute.{type Attribute}
import lustre/element.{type Element} as _
import sketch.{type Class}
import sketch/magic/element.{element, element_}

//

pub fn html(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("html", attributes, children)
}

pub fn base(attributes: List(Attribute(b))) -> Element(b) {
  element_("base", attributes, [])
}

pub fn head(
  attributes: List(Attribute(c)),
  children: List(Element(c)),
) -> Element(c) {
  element_("head", attributes, children)
}

pub fn link(attributes: List(Attribute(d))) -> Element(d) {
  element_("link", attributes, [])
}

pub fn meta(attributes: List(Attribute(e))) -> Element(e) {
  element_("meta", attributes, [])
}

pub fn style(attributes: List(Attribute(f)), child: String) -> Element(f) {
  element_("style", attributes, [text(child)])
}

pub fn title(attributes: List(Attribute(g)), title: String) -> Element(g) {
  element_("title", attributes, [text(title)])
}

//

pub fn text(content: String) -> Element(h) {
  element.text(content)
}

//

pub fn a(
  class: Class,
  attributes: List(Attribute(i)),
  children: List(Element(i)),
) -> Element(i) {
  element("a", class, attributes, children)
}

pub fn a_(
  attributes: List(Attribute(i)),
  children: List(Element(i)),
) -> Element(i) {
  element_("a", attributes, children)
}

pub fn abbr(
  class: Class,
  attributes: List(Attribute(j)),
  children: List(Element(j)),
) -> Element(j) {
  element("abbr", class, attributes, children)
}

pub fn abbr_(
  attributes: List(Attribute(k)),
  children: List(Element(k)),
) -> Element(k) {
  element_("abbr", attributes, children)
}

pub fn address(
  class: Class,
  attributes: List(Attribute(l)),
  children: List(Element(l)),
) -> Element(l) {
  element("address", class, attributes, children)
}

pub fn address_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("address", attributes, children)
}

pub fn area(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  element("area", class, attributes, [])
}

pub fn area_(attributes: List(Attribute(a))) -> Element(a) {
  element_("area", attributes, [])
}

pub fn article(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("article", class, attributes, children)
}

pub fn article_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("article", attributes, children)
}

pub fn aside(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("aside", class, attributes, children)
}

pub fn aside_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("aside", attributes, children)
}

pub fn audio(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("audio", class, attributes, children)
}

pub fn audio_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("audio", attributes, children)
}

pub fn b(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("b", class, attributes, children)
}

pub fn b_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("b", attributes, children)
}

pub fn bdi(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("bdi", class, attributes, children)
}

pub fn bdi_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("bdi", attributes, children)
}

pub fn bdo(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("bdo", class, attributes, children)
}

pub fn bdo_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("bdo", attributes, children)
}

pub fn blockquote(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("blockquote", class, attributes, children)
}

pub fn blockquote_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("blockquote", attributes, children)
}

pub fn body(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("body", class, attributes, children)
}

pub fn body_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("body", attributes, children)
}

pub fn br(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  element("br", class, attributes, [])
}

pub fn br_(attributes: List(Attribute(a))) -> Element(a) {
  element_("br", attributes, [])
}

pub fn button(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("button", class, attributes, children)
}

pub fn button_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("button", attributes, children)
}

pub fn canvas(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("canvas", class, attributes, children)
}

pub fn canvas_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("canvas", attributes, children)
}

pub fn caption(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("caption", class, attributes, children)
}

pub fn caption_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("caption", attributes, children)
}

pub fn cite(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("cite", class, attributes, children)
}

pub fn cite_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("cite", attributes, children)
}

pub fn code(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("code", class, attributes, children)
}

pub fn code_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("code", attributes, children)
}

pub fn col(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  element("col", class, attributes, [])
}

pub fn col_(attributes: List(Attribute(a))) -> Element(a) {
  element_("col", attributes, [])
}

pub fn colgroup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("colgroup", class, attributes, children)
}

pub fn colgroup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("colgroup", attributes, children)
}

pub fn data(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("data", class, attributes, children)
}

pub fn data_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("data", attributes, children)
}

pub fn datalist(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("datalist", class, attributes, children)
}

pub fn datalist_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("datalist", attributes, children)
}

pub fn dd(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("dd", class, attributes, children)
}

pub fn dd_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("dd", attributes, children)
}

pub fn del(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("del", class, attributes, children)
}

pub fn del_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("del", attributes, children)
}

pub fn details(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("details", class, attributes, children)
}

pub fn details_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("details", attributes, children)
}

pub fn dfn(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("dfn", class, attributes, children)
}

pub fn dfn_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("dfn", attributes, children)
}

pub fn dialog(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("dialog", class, attributes, children)
}

pub fn dialog_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("dialog", attributes, children)
}

pub fn div(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("div", class, attributes, children)
}

pub fn div_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("div", attributes, children)
}

pub fn dl(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("dl", class, attributes, children)
}

pub fn dl_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("dl", attributes, children)
}

pub fn dt(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("dt", class, attributes, children)
}

pub fn dt_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("dt", attributes, children)
}

pub fn em(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("em", class, attributes, children)
}

pub fn em_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("em", attributes, children)
}

pub fn embed(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  element("embed", class, attributes, [])
}

pub fn embed_(attributes: List(Attribute(a))) -> Element(a) {
  element_("embed", attributes, [])
}

pub fn fieldset(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("fieldset", class, attributes, children)
}

pub fn fieldset_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("fieldset", attributes, children)
}

pub fn figcaption(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("figcaption", class, attributes, children)
}

pub fn figcaption_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("figcaption", attributes, children)
}

pub fn figure(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("figure", class, attributes, children)
}

pub fn figure_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("figure", attributes, children)
}

pub fn footer(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("footer", class, attributes, children)
}

pub fn footer_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("footer", attributes, children)
}

pub fn form(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("form", class, attributes, children)
}

pub fn form_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("form", attributes, children)
}

pub fn h1(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("h1", class, attributes, children)
}

pub fn h1_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("h1", attributes, children)
}

pub fn h2(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("h2", class, attributes, children)
}

pub fn h2_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("h2", attributes, children)
}

pub fn h3(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("h3", class, attributes, children)
}

pub fn h3_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("h3", attributes, children)
}

pub fn h4(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("h4", class, attributes, children)
}

pub fn h4_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("h4", attributes, children)
}

pub fn h5(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("h5", class, attributes, children)
}

pub fn h5_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("h5", attributes, children)
}

pub fn h6(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("h6", class, attributes, children)
}

pub fn h6_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("h6", attributes, children)
}

pub fn header(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("header", class, attributes, children)
}

pub fn header_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("header", attributes, children)
}

pub fn hgroup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("hgroup", class, attributes, children)
}

pub fn hgroup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("hgroup", attributes, children)
}

pub fn hr(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  element("hr", class, attributes, [])
}

pub fn hr_(attributes: List(Attribute(a))) -> Element(a) {
  element_("hr", attributes, [])
}

pub fn i(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("i", class, attributes, children)
}

pub fn i_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("i", attributes, children)
}

pub fn iframe(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("iframe", class, attributes, children)
}

pub fn iframe_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("iframe", attributes, children)
}

pub fn img(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  element("img", class, attributes, [])
}

pub fn img_(attributes: List(Attribute(a))) -> Element(a) {
  element_("img", attributes, [])
}

pub fn input(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  element("input", class, attributes, [])
}

pub fn input_(attributes: List(Attribute(a))) -> Element(a) {
  element_("input", attributes, [])
}

pub fn ins(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("ins", class, attributes, children)
}

pub fn ins_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("ins", attributes, children)
}

pub fn kbd(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("kbd", class, attributes, children)
}

pub fn kbd_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("kbd", attributes, children)
}

pub fn label(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("label", class, attributes, children)
}

pub fn label_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("label", attributes, children)
}

pub fn legend(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("legend", class, attributes, children)
}

pub fn legend_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("legend", attributes, children)
}

pub fn li(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("li", class, attributes, children)
}

pub fn li_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("li", attributes, children)
}

pub fn main(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("main", class, attributes, children)
}

pub fn main_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("main", attributes, children)
}

pub fn map(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("map", class, attributes, children)
}

pub fn map_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("map", attributes, children)
}

pub fn mark(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("mark", class, attributes, children)
}

pub fn mark_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("mark", attributes, children)
}

pub fn math(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("math", class, attributes, children)
}

pub fn math_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("math", attributes, children)
}

pub fn menu(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("menu", class, attributes, children)
}

pub fn menu_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("menu", attributes, children)
}

pub fn meter(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("meter", class, attributes, children)
}

pub fn meter_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("meter", attributes, children)
}

pub fn nav(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("nav", class, attributes, children)
}

pub fn nav_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("nav", attributes, children)
}

pub fn noscript(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("noscript", class, attributes, children)
}

pub fn noscript_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("noscript", attributes, children)
}

pub fn object(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("object", class, attributes, children)
}

pub fn object_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("object", attributes, children)
}

pub fn ol(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("ol", class, attributes, children)
}

pub fn ol_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("ol", attributes, children)
}

pub fn optgroup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("optgroup", class, attributes, children)
}

pub fn optgroup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("optgroup", attributes, children)
}

pub fn option(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("option", class, attributes, children)
}

pub fn option_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("option", attributes, children)
}

pub fn output(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("output", class, attributes, children)
}

pub fn output_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("output", attributes, children)
}

pub fn p(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("p", class, attributes, children)
}

pub fn p_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("p", attributes, children)
}

pub fn picture(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("picture", class, attributes, children)
}

pub fn picture_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("picture", attributes, children)
}

pub fn portal(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("portal", class, attributes, children)
}

pub fn portal_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("portal", attributes, children)
}

pub fn pre(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("pre", class, attributes, children)
}

pub fn pre_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("pre", attributes, children)
}

pub fn progress(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("progress", class, attributes, children)
}

pub fn progress_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("progress", attributes, children)
}

pub fn q(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("q", class, attributes, children)
}

pub fn q_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("q", attributes, children)
}

pub fn rp(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("rp", class, attributes, children)
}

pub fn rp_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("rp", attributes, children)
}

pub fn rt(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("rt", class, attributes, children)
}

pub fn rt_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("rt", attributes, children)
}

pub fn ruby(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("ruby", class, attributes, children)
}

pub fn ruby_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("ruby", attributes, children)
}

pub fn s(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("s", class, attributes, children)
}

pub fn s_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("s", attributes, children)
}

pub fn samp(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("samp", class, attributes, children)
}

pub fn samp_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("samp", attributes, children)
}

pub fn script(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("script", class, attributes, children)
}

pub fn script_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("script", attributes, children)
}

pub fn search(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("search", class, attributes, children)
}

pub fn search_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("search", attributes, children)
}

pub fn section(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("section", class, attributes, children)
}

pub fn section_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("section", attributes, children)
}

pub fn select(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("select", class, attributes, children)
}

pub fn select_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("select", attributes, children)
}

pub fn slot(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("slot", class, attributes, children)
}

pub fn slot_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("slot", attributes, children)
}

pub fn small(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("small", class, attributes, children)
}

pub fn small_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("small", attributes, children)
}

pub fn source(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  element("source", class, attributes, [])
}

pub fn source_(attributes: List(Attribute(a))) -> Element(a) {
  element_("source", attributes, [])
}

pub fn span(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("span", class, attributes, children)
}

pub fn span_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("span", attributes, children)
}

pub fn strong(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("strong", class, attributes, children)
}

pub fn strong_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("strong", attributes, children)
}

pub fn sub(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("sub", class, attributes, children)
}

pub fn sub_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("sub", attributes, children)
}

pub fn summary(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("summary", class, attributes, children)
}

pub fn summary_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("summary", attributes, children)
}

pub fn sup(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("sup", class, attributes, children)
}

pub fn sup_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("sup", attributes, children)
}

pub fn table(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("table", class, attributes, children)
}

pub fn table_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("table", attributes, children)
}

pub fn tbody(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("tbody", class, attributes, children)
}

pub fn tbody_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("tbody", attributes, children)
}

pub fn td(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("td", class, attributes, children)
}

pub fn td_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("td", attributes, children)
}

pub fn template(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("template", class, attributes, children)
}

pub fn template_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("template", attributes, children)
}

pub fn textarea(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("textarea", class, attributes, children)
}

pub fn textarea_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("textarea", attributes, children)
}

pub fn tfoot(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("tfoot", class, attributes, children)
}

pub fn tfoot_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("tfoot", attributes, children)
}

pub fn th(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("th", class, attributes, children)
}

pub fn th_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("th", attributes, children)
}

pub fn thead(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("thead", class, attributes, children)
}

pub fn thead_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("thead", attributes, children)
}

pub fn time(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("time", class, attributes, children)
}

pub fn time_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("time", attributes, children)
}

pub fn tr(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("tr", class, attributes, children)
}

pub fn tr_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("tr", attributes, children)
}

pub fn track(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  element("track", class, attributes, [])
}

pub fn track_(attributes: List(Attribute(a))) -> Element(a) {
  element_("track", attributes, [])
}

pub fn u(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("u", class, attributes, children)
}

pub fn u_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("u", attributes, children)
}

pub fn ul(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("ul", class, attributes, children)
}

pub fn ul_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("ul", attributes, children)
}

pub fn var(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("var", class, attributes, children)
}

pub fn var_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("var", attributes, children)
}

pub fn video(
  class: Class,
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element("video", class, attributes, children)
}

pub fn video_(
  attributes: List(Attribute(a)),
  children: List(Element(a)),
) -> Element(a) {
  element_("video", attributes, children)
}

pub fn wbr(class: Class, attributes: List(Attribute(a))) -> Element(a) {
  element("wbr", class, attributes, [])
}

pub fn wbr_(attributes: List(Attribute(a))) -> Element(a) {
  element_("wbr", attributes, [])
}
