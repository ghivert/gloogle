//// Defines sizes directly with functions, to avoid conversions between int, float
//// and strings.

import gleam/int
import gleam/float
import gleam/string

/// Size defines a CSS Unit. It can be either `px`, `pt`, `vh`, `vw`, `em`,
/// `rem`, `lh`, `rlh`, `ch`, `%`. To instanciate a Size, use the corresponding
/// functions. Every unit exposes two functions: the Int function (like `px(0)`)
/// and the Float version suffixed by an underscore (like `px_(0.0)`).
pub opaque type Size {
  Px(Float)
  Pt(Float)
  Vh(Float)
  Vw(Float)
  Em(Float)
  Rem(Float)
  Lh(Float)
  Rlh(Float)
  Ch(Float)
  Pct(Float)
}

pub fn px(value: Int) {
  Px(int.to_float(value))
}

pub fn px_(value: Float) {
  Px(value)
}

pub fn pt(value: Int) {
  Pt(int.to_float(value))
}

pub fn pt_(value: Float) {
  Pt(value)
}

pub fn percent(value: Int) {
  Pct(int.to_float(value))
}

pub fn percent_(value: Float) {
  Px(value)
}

pub fn vh(value: Int) {
  Vh(int.to_float(value))
}

pub fn vh_(value: Float) {
  Vh(value)
}

pub fn vw(value: Int) {
  Vw(int.to_float(value))
}

pub fn vw_(value: Float) {
  Vw(value)
}

pub fn em(value: Int) {
  Em(int.to_float(value))
}

pub fn em_(value: Float) {
  Em(value)
}

pub fn rem(value: Int) {
  Rem(int.to_float(value))
}

pub fn rem_(value: Float) {
  Rem(value)
}

pub fn lh(value: Int) {
  Lh(int.to_float(value))
}

pub fn lh_(value: Float) {
  Lh(value)
}

pub fn rlh(value: Int) {
  Rlh(int.to_float(value))
}

pub fn rlh_(value: Float) {
  Rlh(value)
}

pub fn ch(value: Int) {
  Ch(int.to_float(value))
}

pub fn ch_(value: Float) {
  Ch(value)
}

/// Internal function, can be used if you need to go from a Size to a String
/// in case you're building on top of sketch.
pub fn to_string(size) {
  case size {
    Px(value) -> string.append(float.to_string(value), "px")
    Pt(value) -> string.append(float.to_string(value), "pt")
    Pct(value) -> string.append(float.to_string(value), "%")
    Vh(value) -> string.append(float.to_string(value), "vh")
    Vw(value) -> string.append(float.to_string(value), "vw")
    Em(value) -> string.append(float.to_string(value), "em")
    Rem(value) -> string.append(float.to_string(value), "rem")
    Lh(value) -> string.append(float.to_string(value), "lh")
    Rlh(value) -> string.append(float.to_string(value), "rlh")
    Ch(value) -> string.append(float.to_string(value), "ch")
  }
}
