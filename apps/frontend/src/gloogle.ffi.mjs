function findChild(shadowRoot, id) {
  for (const node of shadowRoot.querySelectorAll('search-result')) {
    const elem = node.shadowRoot.getElementById(id)
    if (elem) return elem
  }
}

export function scrollTo(id) {
  return function (_) {
    const cache = document.getElementsByTagName('lazy-node')
    if (!cache?.[0]) return
    const elem = findChild(cache[0].shadowRoot, id)
    if (!elem) return
    const elemRect = elem.getBoundingClientRect()
    const navbarRect = document
      .getElementsByClassName('navbar')?.[0]
      ?.getBoundingClientRect()
    const bodyRect = document.body.getBoundingClientRect()
    const offset = elemRect.top - bodyRect.top - (navbarRect?.height ?? 0) - 12
    window.scrollTo({ top: offset, behavior: 'smooth' })
  }
}

export function captureMessage(content) {
  if (is_dev()) return content
  if (
    typeof Sentry !== 'undefined' &&
    Sentry?.captureMessage &&
    typeof Sentry.captureMessage === 'function'
  )
    Sentry.captureMessage(content)
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
    if (event.key === 'Escape') return callback(event.key)
    if ((!event.metaKey && !event.ctrlKey) || event.key !== 'k') return
    callback(event.key)
  })
}

export function focus(id) {
  const element = document.getElementById(id)
  if (element) {
    element.focus()
    element.select()
  }
}

export function unfocus() {
  const element = document.activeElement
  if (element) {
    element.blur()
  }
}

export function isMac() {
  return (
    navigator.platform.indexOf('Mac') === 0 || navigator.platform === 'iPhone'
  )
}

export function isMobile() {
  return window.matchMedia('(max-width: 700px)').matches
}

export function subscribeIsMobile(callback) {
  window.matchMedia('(max-width: 700px)').addEventListener('change', event => {
    if (event.matches) {
      callback(true)
    } else {
      callback(false)
    }
  })
}
