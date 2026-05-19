import * as $decode from '../gleam_stdlib/gleam/dynamic/decode.mjs'

// Get `Decoder` type back, because it's opaque. Opaque types are not exported,
// but `decode.success` returns a `Decoder(a)`. Since it's an instance of the
// object, it's possible to get back the class directly by accessing
// `constructor`.
const Decoder = $decode.success('').constructor

// Create a `Decoder(a)`, from a decoding function
// `fn (Dynamic) -> #(a, List(DecodeError))`.
// That function replaces the `Decode` constructor in Gleam, and can be used
// transparently.
export const newDecoder = fun => new Decoder(fun)

// Define the `decode_json` function in `decrypt`, which will be wrapped in a
// `Decoder(a)`. `decode_json :: fn (Dynamic) -> #(a, List(DecodeError))`.
export const decodeJSON = (decoder, data) => decoder.function(parse(data))

// Converts a data from a string to a JSON structure. If the string is not a
// valid JSON, or if the data is not a string, returns the data as-is, as it
// can be a valid data structure according to the decoder.
function parse(data) {
  if (typeof data !== 'string') return data
  try {
    return JSON.parse(data)
  } catch (error) {
    return data
  }
}
