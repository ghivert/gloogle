import * as gleam from "./gleam.mjs"

export function coerce(a) {
  return a
}

export function areReferentiallyEqual(a, b) {
  return a === b && gleam.isEqual(a, b)
}
