import gleam/dict
import gleam/list
import gleam/result
import gleam/string
import lustre/element as el
import sketch.{type Cache}
import sketch/internals/class
import sketch/lustre/element

/// Wrap the view function in lustre. Be careful, on BEAM, sketch will add an
/// additional `div` at the root of the HTML tree, to inject the styles in the app.
/// This should have no impact on your app.
pub fn compose(view: fn(model) -> element.Element(msg), cache: Cache) {
  fn(model: model) -> el.Element(msg) {
    let node = view(model)
    let #(cache, node) = element.unstylify(cache, node)
    let content =
      dict.values(cache.cache)
      |> list.flat_map(class.definitions)
      |> string.join("\n")
    let style = el.element("style", [], [el.text(content)])
    el.fragment([style, node])
  }
}
// fn contains_head(el: Element(a)) {
//   case el {
//     vdom.Element(_, _, "head", _, _, _, _) -> True
//     vdom.Element(_, _, _, _, children, _, _) ->
//       list.fold(children, False, fn(acc, val) { acc || contains_head(val) })
//     // vdom.Fragment(elements, _) ->
//     //   list.fold(elements, False, fn(acc, val) { acc || contains_head(val) })
//     _ -> False
//   }
// }
//
// fn put_in_head(el: Element(a), content: String) {
//   case el {
//     vdom.Element(k, n, "head", a, children, s, v) ->
//       children
//       |> list.append([html.style([], content)])
//       |> vdom.Element(k, n, "head", a, _, s, v)
//     vdom.Element(k, n, "html", a, children, s, v) ->
//       children
//       |> list.map(fn(child) { put_in_head(child, content) })
//       |> vdom.Element(k, n, "html", a, _, s, v)
//     node -> node
//   }
// }

// @target(erlang)
// /// Take an Element, and overloads the content with the correct styles from sketch.
// /// Can only be used on BEAM.
// pub fn ssr(el: Element(a), cache: Cache) -> Element(a) {
//   cache
//   |> sketch.render()
//   |> result.map(fn(content) {
//     case contains_head(el) {
//       True -> put_in_head(el, content)
//       False -> html.div([], [html.style([], content), el])
//     }
//   })
//   |> result.unwrap(el)
// }
