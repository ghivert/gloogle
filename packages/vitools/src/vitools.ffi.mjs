import * as gleam from './gleam.mjs'

export function is_dev() {
  return !!import.meta.env.DEV
}

export function base_url() {
  return import.meta.env.BASE_URL
}

export function get_env(name) {
  const result = import.meta.env[`VITE_${name}`]
  if (result !== undefined) return new gleam.Ok(result)
  return new gleam.Error(undefined)
}
