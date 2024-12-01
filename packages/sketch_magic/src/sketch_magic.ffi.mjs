import * as gleam from './gleam.mjs'

let _cache = null

export function setCache(cache) {
  _cache = cache
  return cache
}

export function getCache() {
  if (!_cache) return new gleam.Error()
  return new gleam.Ok(_cache)
}

export function createCssStyleSheet(root) {
  if (!(root instanceof ShadowRoot)) throw new Error(`root is not a ShadowRoot`)
  const stylesheet = new CSSStyleSheet()
  if (root && root.adoptedStyleSheets) {
    root.adoptedStyleSheets.push(stylesheet)
  } else {
    document.adoptedStyleSheets.push(stylesheet)
  }
  return stylesheet
}

export function setStylesheet(content, stylesheet) {
  stylesheet.replaceSync(content)
}
