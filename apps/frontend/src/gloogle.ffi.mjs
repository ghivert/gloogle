import * as gleam from './gleam.mjs'

function findSearchResultChild(shadowRoot, id) {
  for (const node of shadowRoot.querySelectorAll('search-result')) {
    const elem = node.shadowRoot.getElementById(id)
    if (elem) return elem
  }
}

export function scrollTo(id) {
  return function (_) {
    const cache = document.getElementsByTagName('lazy-node')
    if (!cache?.[0]) return
    const elem = findSearchResultChild(cache[0].shadowRoot, id)
    if (!elem) return
    const elemRect = elem.getBoundingClientRect()
    const navbarRect = document
      .getElementsByClassName('navbar')?.[0]
      ?.getBoundingClientRect()
    const bodyRect = document.body.getBoundingClientRect()
    const top = elemRect.top - bodyRect.top - (navbarRect?.height ?? 0) - 12
    window.scrollTo({ top, behavior: 'smooth' })
  }
}

export function captureMessage(content) {
  const isDev = !!import.meta.env.DEV
  if (isDev) return content
  const isSentryDefined = typeof Sentry !== 'undefined'
  const canCaptureMessage = Sentry?.captureMessage === 'function'
  if (isSentryDefined && canCaptureMessage) Sentry.captureMessage(content)
  return content
}

export function updateTitle(title) {
  document.title = title
}

export function coerce(a) {
  return a
}

export function coerceEvent(a) {
  return a.detail
}

export function subscribeFocus(callback) {
  document.addEventListener('keydown', event => {
    if (event.key === 'Escape') return callback(event)
    if (event.key !== 's') return
    callback(event)
  })
}

export function isActive(id) {
  const element = document.getElementById(id)
  return element === document.activeElement
}

export function focus(id, event) {
  const element = document.getElementById(id)
  event.preventDefault()
  if (element) {
    element.focus()
    element.select()
  }
}

export function blur() {
  const element = document.activeElement
  if (element) {
    element.blur()
  }
}

export function isMobile() {
  return window.matchMedia('(max-width: 700px)').matches
}

export function subscribeIsMobile(callback) {
  window
    .matchMedia('(max-width: 700px)')
    .addEventListener('change', event => callback(!!event.matches))
}

export function eventKey(event) {
  if (event.key) return new gleam.Ok(event.key)
  return new gleam.Error()
}
