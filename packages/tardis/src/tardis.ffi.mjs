import Dict from '../gleam_stdlib/dict.mjs'
import { None, Some } from '../gleam_stdlib/gleam/option.mjs'
import { BitArray, CustomType, List, UtfCodepoint } from './gleam.mjs'
import {
  DataBitArray,
  DataBool,
  DataConstant,
  DataCustomType,
  DataDate,
  DataDict,
  DataFunction,
  DataList,
  DataNil,
  DataNumber,
  DataObject,
  DataRegex,
  DataSet,
  DataString,
  DataTuple,
  DataUtfCodepoint,
} from './tardis/internals/data.mjs'

export function toString(data) {
  return inspect(data)
}

export function inspect(v) {
  const t = typeof v
  if (v === true) return new DataBool('True')
  if (v === false) return new DataBool('False')
  if (v === null) return new DataConstant('//js(null)')
  if (v === undefined) return new DataNil()
  if (t === 'string') return new DataString(JSON.stringify(v))
  if (t === 'bigint' || t === 'number') return new DataNumber(v.toString())
  if (Array.isArray(v)) return new DataTuple(List.fromArray(v.map(inspect)))
  if (v instanceof List) return inspectList(v)
  if (v instanceof UtfCodepoint) return inspectUtfCodepoint(v)
  if (v instanceof BitArray) return inspectBitArray(v)
  if (v instanceof CustomType) return inspectCustomType(v)
  if (v instanceof Dict) return inspectDict(v)
  if (v instanceof Set) return DataSet(List.fromArray([...v].map(inspect)))
  if (v instanceof RegExp) return new DataRegex(`//js(${v})`)
  if (v instanceof Date) return new DataDate(`//js(Date("${v.toISOString()}"))`)
  if (v instanceof Function) {
    const args = []
    for (const i of Array(v.length).keys()) args.push(String.fromCharCode(i + 97))
    return new DataFunction(`//fn(${args.join(', ')}) { ... }`)
  }
  return inspectObject(v)
}

function inspectDict(map) {
  const data = []
  map.forEach((value, key) => data.push([inspect(key), inspect(value)]))
  return new DataDict(List.fromArray(data))
}

function inspectObject(v) {
  const name = Object.getPrototypeOf(v)?.constructor?.name || 'Object'
  const props = []
  for (const k of Object.keys(v)) {
    props.push([inspect(k), inspect(v[k])])
  }
  const head = name === 'Object' ? '' : name + ' '
  return new DataObject(head, List.fromArray(props))
}

function inspectCustomType(record) {
  const props = List.fromArray(
    Object.keys(record).map(label => {
      const value = inspect(record[label])
      return isNaN(parseInt(label)) ? [new Some(label + ': '), value] : [new None(), value]
    })
  )
  return new DataCustomType(record.constructor.name, props)
}

export function inspectList(list) {
  return new DataList(List.fromArray(list.toArray().map(inspect)))
}

export function inspectBitArray(bits) {
  return new DataBitArray(`<<${Array.from(bits.buffer).join(', ')}>>`)
}

export function inspectUtfCodepoint(codepoint) {
  return new DataUtfCodepoint(`//utfcodepoint(${String.fromCodePoint(codepoint.value)})`)
}

export function stringify(v) {
  const t = typeof v
  if (v === true) return 'True'
  if (v === false) return 'False'
  if (v === null) return '//js(null)'
  if (v === undefined) return 'Nil'
  if (t === 'string') return JSON.stringify(v)
  if (t === 'bigint' || t === 'number') return v.toString()
  if (Array.isArray(v)) return `#(${v.map(stringify).join(', ')})`
  if (v instanceof List) return stringifyList(v)
  if (v instanceof UtfCodepoint) return stringifyUtfCodepoint(v)
  if (v instanceof BitArray) return stringifyBitArray(v)
  if (v instanceof CustomType) return stringifyCustomType(v)
  if (v instanceof Dict) return stringifyDict(v)
  if (v instanceof Set) return `//js(Set(${[...v].map(stringify).join(', ')}))`
  if (v instanceof RegExp) return `//js(${v})`
  if (v instanceof Date) return `//js(Date("${v.toISOString()}"))`
  if (v instanceof Function) {
    const args = []
    for (const i of Array(v.length).keys()) args.push(String.fromCharCode(i + 97))
    return `//fn(${args.join(', ')}) { ... }`
  }
  return stringifyObject(v)
}

function stringifyDict(map) {
  let body = 'dict.from_list(['
  let first = true
  map.forEach((value, key) => {
    if (!first) body = body + ', '
    body = body + '#(' + stringify(key) + ', ' + stringify(value) + ')'
    first = false
  })
  return body + '])'
}

function stringifyObject(v) {
  const name = Object.getPrototypeOf(v)?.constructor?.name || 'Object'
  const props = []
  for (const k of Object.keys(v)) {
    props.push(`${stringify(k)}: ${stringify(v[k])}`)
  }
  const body = props.length ? ' ' + props.join(', ') + ' ' : ''
  const head = name === 'Object' ? '' : name + ' '
  return `//js(${head}{${body}})`
}

function stringifyCustomType(record) {
  const props = Object.keys(record)
    .map(label => {
      const value = stringify(record[label])
      return isNaN(parseInt(label)) ? `${label}: ${value}` : value
    })
    .join(', ')
  return props ? `${record.constructor.name}(${props})` : record.constructor.name
}

export function stringifyList(list) {
  return `[${list.toArray().map(stringify).join(', ')}]`
}

export function stringifyBitArray(bits) {
  return `<<${Array.from(bits.buffer).join(', ')}>>`
}

export function stringifyUtfCodepoint(codepoint) {
  return `//utfcodepoint(${String.fromCodePoint(codepoint.value)})`
}

export function base16_encode(bit_array) {
  let result = ''
  for (const byte of bit_array.buffer) {
    result += byte.toString(16).padStart(2, '0').toUpperCase()
  }
  return result
}

export function base16_decode(string) {
  const bytes = new Uint8Array(string.length / 2)
  for (let i = 0; i < string.length; i += 2) {
    const a = parseInt(string[i], 16)
    const b = parseInt(string[i + 1], 16)
    if (isNaN(a) || isNaN(b)) return new Error(Nil)
    bytes[i / 2] = a * 16 + b
  }
  return new Ok(new BitArray(bytes))
}

export function isDarkTheme() {
  const matches = matchMedia('(prefers-color-scheme: dark)')
  return matches.matches
}

export function addCustomStyles(content) {
  const stylesheet = new CSSStyleSheet()
  stylesheet.replace(content)
  document.adoptedStyleSheets.push(stylesheet)
}

export function updateLustre(application, initMapper, updateMapper) {
  return application.withFields({
    update: updateMapper(application.update),
    init: initMapper(application.init)
  })
}
