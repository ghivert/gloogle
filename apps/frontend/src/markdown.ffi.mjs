import showdown from 'showdown'
import showdownHighlight from 'showdown-highlight'

export function convert(content) {
  const converter = new showdown.Converter({
    extensions: [showdownHighlight({ pre: true, auto_detection: true })],
    tasklists: true,
  })
  return converter.makeHtml(content)
}
