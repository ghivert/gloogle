import gleam/dynamic/decode
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import palabres
import pog
import simplifile
import tom

pub type Error {
  DatabaseError(pog.QueryError)
  HttpcError(httpc.HttpError)
  JsonError(json.DecodeError)
  SimplifileError(simplifile.FileError, String)
  UnknownError(String)
  ParseTomlError(tom.ParseError)
  GetTomlError(tom.GetError)
  EmptyError
  ActorError(actor.StartError)
}

pub fn empty() {
  Error(EmptyError)
}

pub fn new(message: String) {
  Error(UnknownError(message))
}

pub fn from_option(value: Option(a), message: String) -> Result(a, Error) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(UnknownError(message))
  }
}

pub fn replace_nil(res, message: String) {
  result.replace_error(res, UnknownError(message))
}

pub fn log_dynamic_error(error: decode.DecodeError) {
  palabres.log_warning("Dynamic Decode Error")
  palabres.log_warning("  expected: " <> error.expected)
  palabres.log_warning("  found: " <> error.found)
  palabres.log_warning("  path: " <> string.join(error.path, " / "))
}

pub fn log_decode_error(error: json.DecodeError) {
  case error {
    json.UnexpectedEndOfInput -> palabres.log_warning("Unexpected end of input")
    json.UnexpectedByte(byte) -> {
      palabres.log_warning("Unexpected byte")
      palabres.log_warning("  byte: " <> byte)
    }
    json.UnexpectedSequence(byte) -> {
      palabres.log_warning("Unexpected sequence")
      palabres.log_warning("  byte: " <> byte)
    }
    json.UnableToDecode(errors) -> {
      palabres.log_warning("Unexpected format")
      list.map(errors, log_dynamic_error)
      Nil
    }
  }
}

pub fn log_error(error: Error) {
  case error {
    EmptyError -> Nil
    ActorError(error) -> palabres.log_warning(string.inspect(error))
    HttpcError(_dyn) -> palabres.log_warning("Fetch error")
    DatabaseError(error) -> {
      palabres.log_warning("Query error")
      log_pog_error(error)
    }
    JsonError(error) -> {
      palabres.log_warning("JSON error")
      log_decode_error(error)
    }
    SimplifileError(error, filepath) -> {
      palabres.log_warning("Simplifile error")
      palabres.log_warning("  filepath: " <> filepath)
      log_simplifile(error)
    }
    UnknownError(error) -> {
      palabres.log_warning("Unknown error")
      palabres.log_warning("  error: " <> error)
    }
    ParseTomlError(error) -> {
      palabres.log_warning("Parse Toml Error")
      log_parse_tom_error(error)
    }
    GetTomlError(error) -> {
      palabres.log_warning("Get Toml Error")
      log_get_tom_error(error)
    }
  }
}

pub fn debug_log(error: Error) {
  log_error(error)
  error
}

pub fn log_parse_tom_error(error: tom.ParseError) {
  case error {
    tom.Unexpected(got, expected) -> {
      palabres.log_warning("Unexpected TOML error")
      palabres.log_warning("  got: " <> got)
      palabres.log_warning("  expected: " <> expected)
    }
    tom.KeyAlreadyInUse(key) -> {
      palabres.log_warning("Key already in use")
      palabres.log_warning("  key: " <> string.join(key, "/"))
    }
  }
}

pub fn log_get_tom_error(error: tom.GetError) {
  case error {
    tom.NotFound(key) -> {
      palabres.log_warning("Key not found")
      palabres.log_warning("  key: " <> string.join(key, "/"))
    }
    tom.WrongType(key, expected, got) -> {
      palabres.log_warning("Wrong type")
      palabres.log_warning("  key: " <> string.join(key, "/"))
      palabres.log_warning("  got: " <> got)
      palabres.log_warning("  expected: " <> expected)
    }
  }
}

pub fn log_simplifile(error: simplifile.FileError) {
  case error {
    simplifile.Eacces -> palabres.log_warning("Eacces")
    simplifile.Eagain -> palabres.log_warning("Eagain")
    simplifile.Ebadf -> palabres.log_warning("Ebadf")
    simplifile.Ebadmsg -> palabres.log_warning("Ebadmsg")
    simplifile.Ebusy -> palabres.log_warning("Ebusy")
    simplifile.Edeadlk -> palabres.log_warning("Edeadlk")
    simplifile.Edeadlock -> palabres.log_warning("Edeadlock")
    simplifile.Edquot -> palabres.log_warning("Edquot")
    simplifile.Eexist -> palabres.log_warning("Eexist")
    simplifile.Efault -> palabres.log_warning("Efault")
    simplifile.Efbig -> palabres.log_warning("Efbig")
    simplifile.Eftype -> palabres.log_warning("Eftype")
    simplifile.Eintr -> palabres.log_warning("Eintr")
    simplifile.Einval -> palabres.log_warning("Einval")
    simplifile.Eio -> palabres.log_warning("Eio")
    simplifile.Eisdir -> palabres.log_warning("Eisdir")
    simplifile.Eloop -> palabres.log_warning("Eloop")
    simplifile.Emfile -> palabres.log_warning("Emfile")
    simplifile.Emlink -> palabres.log_warning("Emlink")
    simplifile.Emultihop -> palabres.log_warning("Emultihop")
    simplifile.Enametoolong -> palabres.log_warning("Enametoolong")
    simplifile.Enfile -> palabres.log_warning("Enfile")
    simplifile.Enobufs -> palabres.log_warning("Enobufs")
    simplifile.Enodev -> palabres.log_warning("Enodev")
    simplifile.Enolck -> palabres.log_warning("Enolck")
    simplifile.Enolink -> palabres.log_warning("Enolink")
    simplifile.Enoent -> palabres.log_warning("Enoent")
    simplifile.Enomem -> palabres.log_warning("Enomem")
    simplifile.Enospc -> palabres.log_warning("Enospc")
    simplifile.Enosr -> palabres.log_warning("Enosr")
    simplifile.Enostr -> palabres.log_warning("Enostr")
    simplifile.Enosys -> palabres.log_warning("Enosys")
    simplifile.Enotblk -> palabres.log_warning("Enotblk")
    simplifile.Enotdir -> palabres.log_warning("Enotdir")
    simplifile.Enotsup -> palabres.log_warning("Enotsup")
    simplifile.Enxio -> palabres.log_warning("Enxio")
    simplifile.Eopnotsupp -> palabres.log_warning("Eopnotsupp")
    simplifile.Eoverflow -> palabres.log_warning("Eoverflow")
    simplifile.Eperm -> palabres.log_warning("Eperm")
    simplifile.Epipe -> palabres.log_warning("Epipe")
    simplifile.Erange -> palabres.log_warning("Erange")
    simplifile.Erofs -> palabres.log_warning("Erofs")
    simplifile.Espipe -> palabres.log_warning("Espipe")
    simplifile.Esrch -> palabres.log_warning("Esrch")
    simplifile.Estale -> palabres.log_warning("Estale")
    simplifile.Etxtbsy -> palabres.log_warning("Etxtbsy")
    simplifile.Exdev -> palabres.log_warning("Exdev")
    simplifile.NotUtf8 -> palabres.log_warning("NotUtf8")
    simplifile.Unknown(_) -> palabres.log_warning("Unknown")
  }
}

pub fn log_pog_error(error: pog.QueryError) {
  case error {
    pog.ConstraintViolated(message, constraint, details) -> {
      palabres.log_warning("Constraint violated")
      palabres.log_warning("  message: " <> message)
      palabres.log_warning("  constraint: " <> constraint)
      palabres.log_warning("  details: " <> details)
    }
    pog.PostgresqlError(code, name, message) -> {
      let code = result.unwrap(pog.error_code_name(code), code)
      palabres.log_warning("PostgreSQL error")
      palabres.log_warning("  error: " <> code)
      palabres.log_warning("  name: " <> name)
      palabres.log_warning("  message: " <> message)
    }
    pog.UnexpectedArgumentCount(expected, got) -> {
      palabres.log_warning("Unexpected argument count")
      palabres.log_warning("  expected: " <> int.to_string(expected))
      palabres.log_warning("  got: " <> int.to_string(got))
    }
    pog.UnexpectedArgumentType(expected, got) -> {
      palabres.log_warning("Unexpected argument type")
      palabres.log_warning("  expected: " <> expected)
      palabres.log_warning("  got: " <> got)
    }
    pog.UnexpectedResultType(error) -> {
      palabres.log_warning("Unexpected result type")
      list.map(error, log_dynamic_error)
      Nil
    }
    pog.QueryTimeout -> {
      palabres.log_warning("Query timeout")
    }
    pog.ConnectionUnavailable -> palabres.log_warning("Connection unavailable")
  }
}
