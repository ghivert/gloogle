export function is_dev() {
  return import.meta.env.DEV
}

export function scrollTo(id) {
  const elem = document.getElementById(id)
  if (!elem) return
  const elemRect = elem.getBoundingClientRect()
  const navbarRect = document.getElementsByClassName('navbar')?.[0]?.getBoundingClientRect()
  const bodyRect = document.body.getBoundingClientRect()
  const offset = elemRect.top - bodyRect.top - (navbarRect?.height ?? 0) - 12
  window.scrollTo({ top: offset, behavior: 'smooth' })
}
