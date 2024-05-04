import gleam/dynamic
import gleam/int
import gleam/json
import gleam/list
import gleam/pgo
import gleam/result
import gleam/string
import simplifile
import tom
import wisp

pub type Error {
  DatabaseError(pgo.QueryError)
  FetchError(dynamic.Dynamic)
  JsonError(json.DecodeError)
  SimplifileError(simplifile.FileError, String)
  UnknownError(String)
  ParseTomlError(tom.ParseError)
  GetTomlError(tom.GetError)
}

pub fn log_dynamic_error(error: dynamic.DecodeError) {
  wisp.log_warning("Dynamic Decode Error")
  wisp.log_warning("  expected: " <> error.expected)
  wisp.log_warning("  found: " <> error.found)
  wisp.log_warning("  path: " <> string.join(error.path, " / "))
}

pub fn log_decode_error(error: json.DecodeError) {
  case error {
    json.UnexpectedEndOfInput -> wisp.log_warning("Unexpected end of input")
    json.UnexpectedByte(byte, position) -> {
      wisp.log_warning("Unexpected byte")
      wisp.log_warning("  byte: " <> byte)
      wisp.log_warning("  position: " <> int.to_string(position))
    }
    json.UnexpectedSequence(byte, position) -> {
      wisp.log_warning("Unexpected sequence")
      wisp.log_warning("  byte: " <> byte)
      wisp.log_warning("  position: " <> int.to_string(position))
    }
    json.UnexpectedFormat(errors) -> {
      wisp.log_warning("Unexpected format")
      list.map(errors, log_dynamic_error)
      Nil
    }
  }
}

pub fn log(error: Error) {
  case error {
    FetchError(_dyn) -> wisp.log_warning("Fetch error")
    DatabaseError(error) -> {
      wisp.log_warning("Query error")
      log_pgo_error(error)
    }
    JsonError(error) -> {
      wisp.log_warning("JSON error")
      log_decode_error(error)
    }
    SimplifileError(error, filepath) -> {
      wisp.log_warning("Simplifile error")
      wisp.log_warning("  filepath: " <> filepath)
      log_simplifile(error)
    }
    UnknownError(error) -> {
      wisp.log_warning("Unknown error")
      wisp.log_warning("  error: " <> error)
    }
    ParseTomlError(error) -> {
      wisp.log_warning("Parse Toml Error")
      log_parse_tom_error(error)
    }
    GetTomlError(error) -> {
      wisp.log_warning("Get Toml Error")
      log_get_tom_error(error)
    }
  }
}

pub fn log_parse_tom_error(error: tom.ParseError) {
  case error {
    tom.Unexpected(got, expected) -> {
      wisp.log_warning("Unexpected TOML error")
      wisp.log_warning("  got: " <> got)
      wisp.log_warning("  expected: " <> expected)
    }
    tom.KeyAlreadyInUse(key) -> {
      wisp.log_warning("Key already in use")
      wisp.log_warning("  key: " <> string.join(key, "/"))
    }
  }
}

pub fn log_get_tom_error(error: tom.GetError) {
  case error {
    tom.NotFound(key) -> {
      wisp.log_warning("Key not found")
      wisp.log_warning("  key: " <> string.join(key, "/"))
    }
    tom.WrongType(key, expected, got) -> {
      wisp.log_warning("Wrong type")
      wisp.log_warning("  key: " <> string.join(key, "/"))
      wisp.log_warning("  got: " <> got)
      wisp.log_warning("  expected: " <> expected)
    }
  }
}

pub fn log_simplifile(error: simplifile.FileError) {
  case error {
    simplifile.Eacces -> wisp.log_warning("Eacces")
    simplifile.Eagain -> wisp.log_warning("Eagain")
    simplifile.Ebadf -> wisp.log_warning("Ebadf")
    simplifile.Ebadmsg -> wisp.log_warning("Ebadmsg")
    simplifile.Ebusy -> wisp.log_warning("Ebusy")
    simplifile.Edeadlk -> wisp.log_warning("Edeadlk")
    simplifile.Edeadlock -> wisp.log_warning("Edeadlock")
    simplifile.Edquot -> wisp.log_warning("Edquot")
    simplifile.Eexist -> wisp.log_warning("Eexist")
    simplifile.Efault -> wisp.log_warning("Efault")
    simplifile.Efbig -> wisp.log_warning("Efbig")
    simplifile.Eftype -> wisp.log_warning("Eftype")
    simplifile.Eintr -> wisp.log_warning("Eintr")
    simplifile.Einval -> wisp.log_warning("Einval")
    simplifile.Eio -> wisp.log_warning("Eio")
    simplifile.Eisdir -> wisp.log_warning("Eisdir")
    simplifile.Eloop -> wisp.log_warning("Eloop")
    simplifile.Emfile -> wisp.log_warning("Emfile")
    simplifile.Emlink -> wisp.log_warning("Emlink")
    simplifile.Emultihop -> wisp.log_warning("Emultihop")
    simplifile.Enametoolong -> wisp.log_warning("Enametoolong")
    simplifile.Enfile -> wisp.log_warning("Enfile")
    simplifile.Enobufs -> wisp.log_warning("Enobufs")
    simplifile.Enodev -> wisp.log_warning("Enodev")
    simplifile.Enolck -> wisp.log_warning("Enolck")
    simplifile.Enolink -> wisp.log_warning("Enolink")
    simplifile.Enoent -> wisp.log_warning("Enoent")
    simplifile.Enomem -> wisp.log_warning("Enomem")
    simplifile.Enospc -> wisp.log_warning("Enospc")
    simplifile.Enosr -> wisp.log_warning("Enosr")
    simplifile.Enostr -> wisp.log_warning("Enostr")
    simplifile.Enosys -> wisp.log_warning("Enosys")
    simplifile.Enotblk -> wisp.log_warning("Enotblk")
    simplifile.Enotdir -> wisp.log_warning("Enotdir")
    simplifile.Enotsup -> wisp.log_warning("Enotsup")
    simplifile.Enxio -> wisp.log_warning("Enxio")
    simplifile.Eopnotsupp -> wisp.log_warning("Eopnotsupp")
    simplifile.Eoverflow -> wisp.log_warning("Eoverflow")
    simplifile.Eperm -> wisp.log_warning("Eperm")
    simplifile.Epipe -> wisp.log_warning("Epipe")
    simplifile.Erange -> wisp.log_warning("Erange")
    simplifile.Erofs -> wisp.log_warning("Erofs")
    simplifile.Espipe -> wisp.log_warning("Espipe")
    simplifile.Esrch -> wisp.log_warning("Esrch")
    simplifile.Estale -> wisp.log_warning("Estale")
    simplifile.Etxtbsy -> wisp.log_warning("Etxtbsy")
    simplifile.Exdev -> wisp.log_warning("Exdev")
    simplifile.NotUtf8 -> wisp.log_warning("NotUtf8")
    simplifile.Unknown -> wisp.log_warning("Unknown")
  }
}

pub fn log_pgo_error(error: pgo.QueryError) {
  case error {
    pgo.ConstraintViolated(message, constraint, details) -> {
      wisp.log_warning("Constraint violated")
      wisp.log_warning("  message: " <> message)
      wisp.log_warning("  constraint: " <> constraint)
      wisp.log_warning("  details: " <> details)
    }
    pgo.PostgresqlError(code, name, message) -> {
      let code = result.unwrap(pgo.error_code_name(code), code)
      wisp.log_warning("PostgreSQL error")
      wisp.log_warning("  error: " <> code)
      wisp.log_warning("  name: " <> name)
      wisp.log_warning("  message: " <> message)
    }
    pgo.UnexpectedArgumentCount(expected, got) -> {
      wisp.log_warning("Unexpected argument count")
      wisp.log_warning("  expected: " <> int.to_string(expected))
      wisp.log_warning("  got: " <> int.to_string(got))
    }
    pgo.UnexpectedArgumentType(expected, got) -> {
      wisp.log_warning("Unexpected argument type")
      wisp.log_warning("  expected: " <> expected)
      wisp.log_warning("  got: " <> got)
    }
    pgo.UnexpectedResultType(error) -> {
      wisp.log_warning("Unexpected result type")
      list.map(error, log_dynamic_error)
      Nil
    }
    pgo.ConnectionUnavailable -> wisp.log_warning("Connection unavailable")
  }
}
