import frontend/discuss
import gleam/io
import gleam/option.{Some}

pub fn describe_http_error(error: discuss.DiscussError) {
  case error {
    discuss.InternalServerError ->
      Some("Internal server error. Please try again later.")
    discuss.NetworkError -> Some("Network error. Please try again later.")
    discuss.NotFound ->
      Some("Resource not found. Make sure you have the correct URL.")
    discuss.InvalidJsonBody -> Some("Invalid JSON body. Please, retry later.")
    discuss.DecodeError(error) -> {
      io.debug(error)
      Some("Format error. Please try again later.")
    }
  }
}
