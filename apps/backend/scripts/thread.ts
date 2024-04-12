export const sleep = (timeout = 2000) => {
  return new Promise((resolve) => {
    setTimeout(resolve, timeout)
  })
}
