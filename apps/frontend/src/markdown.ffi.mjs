import DOMPurify from 'dompurify'
import hljs from 'highlight.js/lib/core'
import { Marked } from 'marked'
import { markedHighlight } from 'marked-highlight'

const parser = new Marked(
  markedHighlight({
    langPrefix: 'hljs language-',
    highlight(code, lang, _info) {
      const language = hljs.getLanguage(lang) ? lang : 'plaintext'
      return hljs.highlight(code, { language }).value
    },
  })
)

export function convert(content) {
  const parsed = parser.parse(content)
  return DOMPurify.sanitize(parsed, { USE_PROFILES: { html: true } })
}
