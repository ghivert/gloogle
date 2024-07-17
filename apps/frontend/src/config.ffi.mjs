export function is_dev() {
  return import.meta.env.DEV
}

function findChild(shadowRoot, id) {
  for (const node of shadowRoot.querySelectorAll('search-result')) {
    const elem = node.shadowRoot.getElementById(id)
    if (elem) return elem
  }
}

export function scrollTo(id) {
  const cache = document.getElementsByTagName('cache-signatures')
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

export function coerce_event(a) {
  return a.detail
}
