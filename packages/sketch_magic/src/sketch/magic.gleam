import gleam/dynamic.{type Dynamic}
import gleam/list
import lustre/element as el
import lustre/element/html
import sketch.{type Cache}
import sketch/internals/ffi

pub type StyleSheet {
  CssStyleSheet(stylesheet: Dynamic)
  NodeStyleSheet
}

pub fn setup(cache: Cache) {
  ffi.set_cache(cache)
  Ok(cache)
}

/// Wrap the view function in lustre. This should have no impact on your app.
pub fn render(stylesheets: List(StyleSheet), view: fn() -> el.Element(msg)) {
  let new_view = view()
  let assert Ok(cache) = ffi.get_cache()
  let content = sketch.render(cache)
  use view, stylesheet <- list.fold(stylesheets, new_view)
  render_stylesheet(content, stylesheet, view)
}

fn render_stylesheet(content, stylesheet, view) {
  case stylesheet {
    CssStyleSheet(stylesheet:) -> render_css(content, stylesheet, view)
    NodeStyleSheet -> render_node(content, view)
  }
}

fn render_css(content, stylesheet, view) {
  ffi.set_stylesheet(content, stylesheet)
  view
}

fn render_node(content, view) {
  let style = html.style([], content)
  el.fragment([style, view])
}

/// Generate a class name from a `Class`, using the provided `Cache` in the
/// environment.
pub fn class_name(class: sketch.Class) {
  let assert Ok(cache) = ffi.get_cache()
  let #(cache, class_name) = sketch.class_name(class, cache)
  ffi.set_cache(cache)
  class_name
}

/// Output the StyleSheet in a `CSSStyleSheet` in `document`.
/// `document` cannot be used on server.
pub fn document() -> StyleSheet {
  let stylesheet = ffi.create_document_stylesheet()
  CssStyleSheet(stylesheet:)
}

/// Output the StyleSheet in a `CSSStyleSheet` in a shadow root.
/// `shadow` cannot be used on server.
pub fn shadow(root: Dynamic) -> StyleSheet {
  let stylesheet = ffi.create_shadow_root_stylesheet(root)
  CssStyleSheet(stylesheet:)
}

pub fn node() -> StyleSheet {
  NodeStyleSheet
}
