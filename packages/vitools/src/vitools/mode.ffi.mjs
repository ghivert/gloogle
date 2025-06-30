import * as gleam from '../gleam.mjs'

export function read() {
  try {
    const mode = import.meta.env.MODE
    if (mode === undefined) return new gleam.Error()
    return new gleam.Ok(mode)
  } catch (error) {
    return new gleam.Error()
  }
}
