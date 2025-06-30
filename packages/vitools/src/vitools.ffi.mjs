import * as gleam from './gleam.mjs'

export function baseURL() {
  try {
    return new gleam.Ok(import.meta.env.BASE_URL)
  } catch (_error) {
    return new gleam.Error()
  }
}

export function getEnv(name) {
  try {
    const result = import.meta.env[`VITE_${name}`]
    if (result === undefined) return new gleam.Error()
    return new gleam.Ok(result)
  } catch (_error) {
    return new gleam.Error()
  }
}
