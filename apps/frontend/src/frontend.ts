// @ts-ignore
import gleamHljs from '@gleam-lang/highlight.js-gleam'
import hljs from 'highlight.js/lib/core'
import plaintext from 'highlight.js/lib/languages/plaintext'
// @ts-ignore
import { main } from './frontend.gleam'

hljs.registerLanguage('gleam', gleamHljs)
hljs.registerLanguage('plaintext', plaintext)
document.addEventListener('DOMContentLoaded', main)
