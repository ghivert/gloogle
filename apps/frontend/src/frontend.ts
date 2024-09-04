// @ts-ignore
import gleamHljs from '@gleam-lang/highlight.js-gleam'
import hljs from 'highlight.js/lib/core'
import plaintext from 'highlight.js/lib/languages/plaintext'
// @ts-ignore
import { LineChart } from './chart.mjs'
// @ts-ignore
import { main } from './frontend.gleam'
import './stylesheets/all.css'
import './stylesheets/hljs-theme.css'
import './stylesheets/main.css'
import './stylesheets/normalize.css'

LineChart.register()
// @ts-ignore
Element.prototype._attachShadow = Element.prototype.attachShadow
Element.prototype.attachShadow = function () {
  // @ts-ignore
  return this._attachShadow({ mode: 'open' })
}

hljs.registerLanguage('gleam', gleamHljs)
hljs.registerLanguage('plaintext', plaintext)
document.addEventListener('DOMContentLoaded', main)
