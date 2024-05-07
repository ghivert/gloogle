import showdown from 'showdown'

export function convert(content) {
  const converter = new showdown.Converter({
    tasklists: true,
  })
  return converter.makeHtml(content)
}
