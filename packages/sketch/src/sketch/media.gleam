//// Define media queries directly with functions.
//// Refer to the sketch module to get more details on the usage.
////
//// ## Advanced usage
////
//// Media queries can be rather complex, and the module tries to give all
//// features in a usable way. A media query takes form (property: value) and
//// can be combined, like (orientation: landscape or min-width: 1000px).
//// Those media queries can be created by using the corresponding `and`,
//// `or` or `not` functions.

import gleam/string
import sketch/size.{type Size, to_string as to_str}

pub opaque type ColorMode {
  Dark
  Light
}

pub opaque type Query {
  MaxWidth(Size)
  MinWidth(Size)
  MaxHeight(Size)
  MinHeight(Size)
  ColorScheme(ColorMode)
  And(Query, Query)
  Or(Query, Query)
  Not(Query)
  Orientation(String)
}

pub fn dark_theme() {
  ColorScheme(Dark)
}

pub fn light_theme() {
  ColorScheme(Light)
}

pub fn max_width(size) {
  MaxWidth(size)
}

pub fn min_width(size) {
  MinWidth(size)
}

pub fn max_height(size) {
  MaxHeight(size)
}

pub fn min_height(size) {
  MinHeight(size)
}

pub fn and(first: Query, second: Query) {
  And(first, second)
}

pub fn or(first: Query, second: Query) {
  Or(first, second)
}

pub fn not(query: Query) {
  Not(query)
}

pub fn landscape() {
  Orientation("landscape")
}

pub fn portrait() {
  Orientation("portrait")
}

fn q_to_str(query: Query) {
  case query {
    ColorScheme(Dark) -> "(prefers-color-scheme: dark)"
    ColorScheme(Light) -> "(prefers-color-scheme: light)"
    MaxWidth(s) -> string.join(["(max-width: ", to_str(s), ")"], "")
    MinWidth(s) -> string.join(["(min-width: ", to_str(s), ")"], "")
    MaxHeight(s) -> string.join(["(max-height: ", to_str(s), ")"], "")
    MinHeight(s) -> string.join(["(min-height: ", to_str(s), ")"], "")
    Orientation(o) -> string.join(["(orientation: ", o, ")"], "")
    Not(q) -> string.append("not ", q_to_str(q))
    And(fst, snd) -> string.join([q_to_str(fst), "and", q_to_str(snd)], " ")
    Or(fst, snd) -> string.join([q_to_str(fst), "or", q_to_str(snd)], " ")
  }
}

/// Internal function, can be used if you need to go from a media query to a String
/// in case you're building on top of sketch.
pub fn to_string(query: Query) {
  let content = q_to_str(query)
  string.append("@media ", content)
}
