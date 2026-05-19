import gleam/dynamic
import gleam/dynamic/decode.{type Decoder}
import gleam/float
import gleam/function
import gleam/int
import gleam/list
import gleam/option.{None}
import gleam/pair
import gleam/string
import gleam/time/calendar
import gleam/time/timestamp
import youid/uuid

/// Decode any number as in `Int`. Integers will be kept as is, while floating
/// numbers will be rounded to the nearest integer. Any number will be
/// converted, whether they're real numbers or numbers turned into strings.
///
/// ```gleam
/// decode.run(dynamic.from(1), decrypt.int())     // -> Ok(1)
/// decode.run(dynamic.from(1.0), decrypt.int())   // -> Ok(1)
/// decode.run(dynamic.from(1.1), decrypt.int())   // -> Ok(1)
/// decode.run(dynamic.from(1.5), decrypt.int())   // -> Ok(2)
/// decode.run(dynamic.from("1"), decrypt.int())   // -> Ok(1)
/// decode.run(dynamic.from("1.0"), decrypt.int()) // -> Ok(1)
/// decode.run(dynamic.from("1.1"), decrypt.int()) // -> Ok(1)
/// decode.run(dynamic.from("1.5"), decrypt.int()) // -> Ok(2)
/// ```
pub fn int() {
  let as_int = decode.float |> decode.map(float.round)
  let int_string = int_string()
  let float_string = float_string() |> decode.map(float.round)
  decode.one_of(decode.int, or: [as_int, int_string, float_string])
  |> decode.collapse_errors("Int")
}

/// Decode any number as in `Float`. Floats will be kept as is, while integers
/// numbers will be turned to float. Any number will be
/// converted, whether they're real numbers or numbers turned into strings.
///
/// ```gleam
/// decode.run(dynamic.from(1), decrypt.float())     // -> Ok(1.0)
/// decode.run(dynamic.from(1.0), decrypt.float())   // -> Ok(1.0)
/// decode.run(dynamic.from(1.1), decrypt.float())   // -> Ok(1.1)
/// decode.run(dynamic.from(1.5), decrypt.float())   // -> Ok(1.5)
/// decode.run(dynamic.from("1"), decrypt.float())   // -> Ok(1.0)
/// decode.run(dynamic.from("1.0"), decrypt.float()) // -> Ok(1.0)
/// decode.run(dynamic.from("1.1"), decrypt.float()) // -> Ok(1.1)
/// decode.run(dynamic.from("1.5"), decrypt.float()) // -> Ok(1.5)
/// ```
pub fn float() {
  let as_float = decode.int |> decode.map(int.to_float)
  let int_string = int_string() |> decode.map(int.to_float)
  let float_string = float_string()
  decode.one_of(decode.float, or: [as_float, float_string, int_string])
  |> decode.collapse_errors("Float")
}

/// Decode an integer in comprised in [1..12] to its corresponding month
/// in `gleam/time/calendar.Month`.
///
/// ```gleam
/// decode.run(dynamic.from(1), decrypt.month())  // -> Ok(calendary.January)
/// decode.run(dynamic.from(2), decrypt.month())  // -> Ok(calendar.February)
/// decode.run(dynamic.from(3), decrypt.month())  // -> Ok(calendar.March)
/// decode.run(dynamic.from(4), decrypt.month())  // -> Ok(calendar.April)
/// decode.run(dynamic.from(5), decrypt.month())  // -> Ok(calendar.May)
/// decode.run(dynamic.from(6), decrypt.month())  // -> Ok(calendar.June)
/// decode.run(dynamic.from(7), decrypt.month())  // -> Ok(calendar.July)
/// decode.run(dynamic.from(8), decrypt.month())  // -> Ok(calendar.August)
/// decode.run(dynamic.from(9), decrypt.month())  // -> Ok(calendar.September)
/// decode.run(dynamic.from(10), decrypt.month()) // -> Ok(calendar.October)
/// decode.run(dynamic.from(11), decrypt.month()) // -> Ok(calendar.November)
/// decode.run(dynamic.from(12), decrypt.month()) // -> Ok(calendar.December)
/// ```
pub fn month() {
  use month <- decode.then(decode.int)
  case month {
    1 -> decode.success(calendar.January)
    2 -> decode.success(calendar.February)
    3 -> decode.success(calendar.March)
    4 -> decode.success(calendar.April)
    5 -> decode.success(calendar.May)
    6 -> decode.success(calendar.June)
    7 -> decode.success(calendar.July)
    8 -> decode.success(calendar.August)
    9 -> decode.success(calendar.September)
    10 -> decode.success(calendar.October)
    11 -> decode.success(calendar.November)
    12 -> decode.success(calendar.December)
    _ -> decode.failure(calendar.January, "Month")
  }
}

/// Decode a `gleam.Timestamp`, whether it is an RFC-3339 string or an Erlang
/// tuple timestamp (`{{year, month, day}, {hours, minutes, seconds}})`.
///
/// ```gleam
/// let time = dynamic.from("2025-03-03T16:28:13.700Z")
/// let tuple = dynamic.from(#(#(2025, 3, 3), #(16, 28, 13)))
/// let tuple_ns = dynamic.from(#(#(2025, 3, 3), #(16, 28, 13.700)))
/// decode.run(time, decrypt.timestamp())  // -> Ok(Timestamp)
/// decode.run(tuple, decrypt.timestamp()) // -> Ok(Timestamp)
/// decode.run(tuple_ns, decrypt.timestamp()) // -> Ok(Timestamp)
/// ```
pub fn timestamp() -> Decoder(timestamp.Timestamp) {
  let tuple = tuple_timestamp()
  let rfc3339 = rfc3339_decoder()
  decode.one_of(tuple, or: [rfc3339])
  |> decode.collapse_errors("Timestamp")
}

/// Decode in the same way a JSON string or a `Dynamic` data structure
/// corresponding to the JSON string already parsed. \
/// Such function can be
/// used in the same way with fields coming from Postgres (JSON string
/// formatted) or coming from HTTP calls (dynamically formatted).
///
/// ```gleam
/// let decoder = decrypt.json(decode.int)
/// let data = json.to_string(json.int(1))
/// json.parse(data, decoder) // -> Ok(1)
/// decode.run(data, decoder) // -> Ok(1)
/// let data = dynamic.from(1)
/// decode.run(data, decoder) // -> Ok(1)
/// ```
pub fn json(decoder: Decoder(a)) -> Decoder(a) {
  decode_json(decoder, _)
  |> new_decoder
}

/// Decode a UUID, whether in string format (`xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`)
/// or as a bit array. Decode it as a string format no matter the target,
/// to ensure wide-compatibility between targets.
///
/// ```gleam
/// import gleam/dynamic
/// import gleam/dynamic/decode
/// import youid/uuid
///
/// let bytes_uuid = uuid.v4() |> uuid.to_bit_array |> dynamic.from
/// let string_uuid = uuid.v4() |> uuid.to_string |> dynamic.from
/// let x = decode.run(bytes_uuid, decrypt.uuid())
/// let y = decode.run(string_uuid, decrypt.uuid())
/// // x : Result(String, decode.DecodeError)
/// // y : Result(String, decode.DecodeError)
/// ```
pub fn uuid() -> Decoder(String) {
  decode.one_of(decode.string, or: [
    decode.then(decode.bit_array, fn(content) {
      case uuid.from_bit_array(content) {
        Ok(uuid) -> decode.success(uuid.to_string(uuid))
        Error(_) -> decode.failure("", "UUID")
      }
    }),
  ])
  |> decode.map(string.lowercase)
  |> decode.collapse_errors("UUID")
}

/// Decode a totally optional field as `option.Option(x)`. Default value is
/// `option.None`. You can always swap `decode.field("x", decode.optional(decoder))`
/// with `decrypt.optional_field("x", decoder)`, as `decrypt.optional_field`
/// is more permissive than the other form, and will automatically fallback
/// on `option.None` when the field is either missing, or the field is optional.
///
/// ```gleam
/// use str: Option(String) <- decrypt.optional_field("field", decode.string)
/// use int: Option(Int) <- decrypt.optional_field("field", decode.int)
/// decode.success(#(str, int))
/// ```
pub fn optional_field(
  field: a,
  decoder: Decoder(b),
  next: fn(option.Option(b)) -> Decoder(c),
) -> Decoder(c) {
  let decoder = decode.optional(decoder)
  decode.optional_field(field, None, decoder, next)
}

/// Decode a list of results, and ignore the errors. Similar to `filter_map`,
/// but for decoders.
///
/// ```gleam
/// decode.nullable(decode.string)
/// |> decode.map(option.to_result(_, Nil))
/// |> decode.filter_list
/// ```
pub fn filter_list(decoder: Decoder(Result(a, b))) -> Decoder(List(a)) {
  use content <- decode.map(decode.list(decoder))
  list.filter_map(content, function.identity)
}

/// Decode a tuple timestamp, following the tuple shape:
/// `{{year, month, day}, {hours, minutes, seconds.nanoseconds}}`.
/// `year >= 0`, `month == [1..12]`, `day == [1..31]`.
/// `hours == [0..23]`, `minutes == [0..59]`,
/// `seconds == [0..59].[0..1_000_000_000]`.
fn tuple_timestamp() -> Decoder(timestamp.Timestamp) {
  use date <- decode.field(0, {
    use year <- decode.field(0, int())
    use month <- decode.field(1, month())
    use day <- decode.field(2, int())
    calendar.Date(year:, month:, day:)
    |> decode.success
  })
  use time <- decode.field(1, {
    use hours <- decode.field(0, int())
    use minutes <- decode.field(1, int())
    use #(seconds, nanoseconds) <- decode.field(2, seconds_decoder())
    calendar.TimeOfDay(hours:, minutes:, seconds:, nanoseconds:)
    |> decode.success
  })
  timestamp.from_calendar(date, time, calendar.utc_offset)
  |> decode.success
}

fn seconds_decoder() -> Decoder(#(Int, Int)) {
  let int = decode.map(decode.int, pair.new(_, 0))
  let float = {
    use f <- decode.map(decode.float)
    let floored = float.floor(f)
    let seconds = float.round(floored)
    let nanoseconds = float.round({ f -. floored } *. 1_000_000_000.0)
    #(seconds, nanoseconds)
  }
  decode.one_of(int, or: [float])
}

/// Decode an RFC-3339 string into a `gleam/time/timestamp.Timestamp`.
/// RFC-3339 is often referred as ISO-8601, but is more strict than the ISO
/// format, whereas ISO format refers to many different (unused) formats.
fn rfc3339_decoder() -> Decoder(timestamp.Timestamp) {
  use content <- decode.then(decode.string)
  case timestamp.parse_rfc3339(content) {
    Ok(time) -> decode.success(time)
    Error(_) -> decode.failure(timestamp.system_time(), "Timestamp")
  }
}

fn int_string() {
  use string <- decode.then(decode.string)
  case int.parse(string) {
    Ok(value) -> decode.success(value)
    Error(_) -> decode.failure(0, "Int")
  }
}

fn float_string() {
  use string <- decode.then(decode.string)
  case float.parse(string) {
    Ok(value) -> decode.success(value)
    Error(_) -> decode.failure(0.0, "Float")
  }
}

/// Decode a potential JSON object. The `data` is whether a JSON string or a
/// `Dynamic` data structure, and will be either parsed as `Dynamic` and
/// decoding, or decoded right away.
@external(erlang, "decrypt_ffi", "decode_json")
@external(javascript, "./decrypt.ffi.mjs", "decodeJSON")
fn decode_json(
  decoder: Decoder(t),
  data: dynamic.Dynamic,
) -> #(t, List(decode.DecodeError))

/// Create a new `Decoder(t)` from a decoding function
/// `fn (Dynamic) -> #(t, List(DecodeError))`. That function exists only
/// because the `Decoder(t)` type is opaque, and as such, runtime abilities
/// should be used to trick the Gleam compiler. \
/// That function should never _ever_ leave the `decrypt` module, nor being
/// leaked out.
@external(erlang, "decrypt_ffi", "new_decoder")
@external(javascript, "./decrypt.ffi.mjs", "newDecoder")
fn new_decoder(
  function: fn(dynamic.Dynamic) -> #(t, List(decode.DecodeError)),
) -> Decoder(t)
