import gleamHljs from '@gleam-lang/highlight.js-gleam'
import hljs from 'highlight.js/lib/core'
import plaintext from 'highlight.js/lib/languages/plaintext'
import * as barChart from './elements/bar_chart.element.mjs'
import * as lineChart from './elements/line_chart.element.mjs'
import { main } from './frontend.gleam'
import './stylesheets/all.css'
import './stylesheets/hljs-theme.css'
import './stylesheets/main.css'
import './stylesheets/normalize.css'

lineChart.register()
barChart.register()

hljs.registerLanguage('gleam', gleamHljs)
hljs.registerLanguage('plaintext', plaintext)

document.addEventListener('DOMContentLoaded', main)
