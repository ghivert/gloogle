export const uid = (function () {
  let id = 0
  const classNames = {}
  return function (className) {
    classNames[className] ??= (id++).toString().padStart(4, '0')
    const index = classNames[className]
    return `css-${index}`
  }
})()

export function getFunctionName() {
  const error = new Error()
  if (!error.stack)
    throw new Error('Unable to find the stacktrace and to infer the className')
  const stack = error.stack ?? ''
  const parts = stack.split('\n')
  const end = parts.findIndex((l) => l.includes('LustreClientApplication'))
  const endIndex = end === -1 ? parts.length : end
  const st = parts.slice(1, endIndex).join('\n')
  return st
}

// Compare two data structures to check if they're the same.
export function deepEqual(a, b) {
  const consts = ['string', 'number', 'boolean']
  if (consts.includes(typeof a) || consts.includes(typeof b)) return a === b
  for (const value in a) {
    if (!(value in b)) return false
    const isSame = deepEqual(a[value], b[value])
    if (!isSame) return false
  }
  return true
}

export function isBrowser() {
  if (typeof window === 'undefined') return false
  if (typeof document === 'undefined') return false
  return true
}
