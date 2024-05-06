import gleam/dynamic
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/string
import lustre_http as http

pub fn describe_json_error(error: json.DecodeError) {
  case error {
    json.UnexpectedEndOfInput ->
      "Impossible to parse the response. Unexpected end of input."
    json.UnexpectedByte(byte, position) -> {
      "Impossible to parse the response. Unexpected byte. "
      <> { byte <> " at position " <> int.to_string(position) }
    }
    json.UnexpectedSequence(byte, position) -> {
      "Impossible to parse the response. Unexpected sequence. "
      <> { byte <> " at position " <> int.to_string(position) }
    }
    json.UnexpectedFormat(errors) -> {
      string.append("Impossible to parse the response. Unexpected format. ", {
        errors
        |> list.map(fn(error) {
          let dynamic.DecodeError(expected, found, path) = error
          let expected = "Expected value: " <> expected
          let found = "Found: " <> found
          let path = "Path: " <> string.join(path, ".")
          expected <> ". " <> found <> ". " <> path <> "."
        })
        |> string.join("\n")
      })
    }
  }
  |> Some
}

pub fn describe_http_error(error: http.HttpError) {
  case error {
    http.BadUrl(url) -> Some("Bad URL: " <> url)
    http.InternalServerError(error) ->
      Some({ "Internal server error. Please try again later. " <> error })
    http.JsonError(error) -> describe_json_error(error)
    http.NetworkError -> Some("Network error. Please try again later.")
    http.NotFound -> Some("Resource not found. Please try again later.")
    http.OtherError(error_code, description) ->
      Some("Error " <> int.to_string(error_code) <> ". " <> description)
    http.Unauthorized -> Some("Operation unauthorized.")
  }
}
