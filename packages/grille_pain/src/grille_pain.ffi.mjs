let storeDispatcher_ = null

export function storeDispatcher(dispatcher) {
  if (storeDispatcher_ !== null) throw new Error("You should not instanciate two grille_pain instance")
  storeDispatcher_ = dispatcher
  return dispatcher
}

export function getDispatcher() {
  if (storeDispatcher_ === null) throw new Error("You should instanciate grille_pain")
  return storeDispatcher_
}

export function isDarkTheme() {
  const matches = matchMedia('(prefers-color-scheme: dark)')
  return matches.matches
}

export function computeBottomPosition() {
  const [...nodes] = document.getElementsByClassName('grille_pain-toast')
  return nodes.reduce((acc, node) => {
    if (node.classList.contains('grille_pain-toast-hidden')) return acc
    const dimensions = node.getBoundingClientRect()
    return acc + dimensions.height - 12;
  }, 0)
}

export function computeToastSize(id) {
  const node = document.getElementsByClassName(`grille_pain-toast-${id}`)
  if (node && node[0]) {
    if (node[0].classList.contains('grille_pain-toast-visible'))
      return node[0].getBoundingClientRect().height - 12
  }
  return 0
}

export function addKeyframe(shadowRoot) {
  const stylesheet = new CSSStyleSheet()
  stylesheet.replace(`@keyframes progress_bar {
  from {
    width: 100%;
  }

  to {
    width: 0%;
  }
}`)
  shadowRoot.adoptedStyleSheets.push(stylesheet)
}
