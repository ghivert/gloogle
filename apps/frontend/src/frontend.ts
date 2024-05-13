// @ts-ignore
import gleamHljs from '@gleam-lang/highlight.js-gleam'
import hljs from 'highlight.js'
// @ts-ignore
import { main } from './frontend.gleam'

hljs.registerLanguage('gleam', gleamHljs)
document.addEventListener('DOMContentLoaded', main)
