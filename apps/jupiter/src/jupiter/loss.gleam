import gleam/dynamic/decode
import gleam/erlang/process
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

pub type Loss(a) =
  Result(a, Error)

pub type Error {
  ActorError(actor.StartError)
  CustomError(String)
  DatabaseError(pog.QueryError)
  EmptyError
  HttpcError(httpc.HttpError)
  JsonError(json.DecodeError)
  SimplifileError(simplifile.FileError, String)
  TomlGetError(tom.GetError)
  TomlParseError(tom.ParseError)
}

pub fn empty() {
  EmptyError
  |> Error
}

pub fn new(message: String) {
  message
  |> CustomError
  |> Error
}

pub fn from_option(value: Option(a), message: String) -> Loss(a) {
  case value {
    Some(value) -> Ok(value)
    None -> Error(CustomError(message))
  }
}

pub fn dismiss(res: Loss(a)) -> Nil {
  case res {
    Ok(_) -> Nil
    Error(e) -> log(e)
  }
}

pub fn log(error: Error) {
  case error {
    EmptyError -> Nil
    ActorError(error) -> {
      palabres.warning("Actor error")
      |> palabres.string("error", "actor.StartError")
      |> palabres.string("type", {
        case error {
          actor.InitTimeout -> "Init timeout"
          actor.InitFailed(failure) -> "Init failed: " <> failure
          actor.InitExited(process.Normal) -> "Exit Normal"
          actor.InitExited(process.Killed) -> "Exit Killed"
          actor.InitExited(process.Abnormal(reason:)) ->
            "Exit Abnormal: " <> string.inspect(reason)
        }
      })
      |> palabres.log
    }
    HttpcError(dyn) -> {
      palabres.warning("Fetch error")
      |> palabres.string("error", "httpc.HttpError")
      |> palabres.string("type", {
        case dyn {
          httpc.InvalidUtf8Response -> "Invalid UTF-8 Response"
          httpc.FailedToConnect(..) -> "Failed to connect"
          httpc.ResponseTimeout -> "Response timeout"
        }
      })
      |> palabres.log
    }
    DatabaseError(error) -> {
      palabres.warning("Query error")
      |> palabres.string("error", "pog.QueryError")
      |> log_pog_error(error)
      |> palabres.log
    }
    JsonError(error) -> {
      palabres.warning("JSON error")
      |> palabres.string("error", "json.DecodeError")
      |> log_decode_error(error)
      |> palabres.log
    }
    SimplifileError(error, filepath) -> {
      palabres.warning("Simplifile error")
      |> palabres.string("error", "simplifile.FileError")
      |> palabres.string("filepath", filepath)
      |> palabres.string("code", string.lowercase(string.inspect(error)))
      |> palabres.log
    }
    CustomError(error) -> {
      palabres.warning("Unknown error")
      |> palabres.string("error", "CustomError")
      |> palabres.string("content", error)
      |> palabres.log
    }
    TomlParseError(error) -> {
      palabres.warning("Parse Toml Error")
      |> palabres.string("error", "tom.ParseError")
      |> log_parse_tom_error(error)
      |> palabres.log
    }
    TomlGetError(error) -> {
      palabres.warning("Get Toml Error")
      |> palabres.string("error", "tom.GetError")
      |> log_get_tom_error(error)
      |> palabres.log
    }
  }
}

fn log_dynamic_error(log: palabres.Log, errors: List(decode.DecodeError)) {
  use log, error, index <- list.index_fold(errors, log)
  let index = int.to_string(index)
  palabres.string(log, "error_" <> index, {
    json.to_string({
      json.object([
        #("expected", json.string(error.expected)),
        #("found", json.string(error.found)),
        #("path", json.array(error.path, json.string)),
      ])
    })
  })
}

fn log_decode_error(log: palabres.Log, error: json.DecodeError) {
  case error {
    json.UnexpectedEndOfInput ->
      palabres.string(log, "type", "Unexpected end of input")
    json.UnexpectedByte(byte) ->
      log
      |> palabres.string("type", "Unexpected byte")
      |> palabres.string("byte", byte)
    json.UnexpectedSequence(byte) ->
      log
      |> palabres.string("type", "Unexpected sequence")
      |> palabres.string("byte", byte)
    json.UnableToDecode(errors) ->
      log
      |> palabres.string("type", "Unable to decode")
      |> log_dynamic_error(errors)
  }
}

fn log_parse_tom_error(log: palabres.Log, error: tom.ParseError) {
  case error {
    tom.Unexpected(got, expected) ->
      log
      |> palabres.string("type", "Unexpected TOML error")
      |> palabres.string("got", got)
      |> palabres.string("expected", expected)
    tom.KeyAlreadyInUse(key) ->
      log
      |> palabres.string("type", "Key already in use")
      |> palabres.string("key", string.join(key, "/"))
  }
}

fn log_get_tom_error(log: palabres.Log, error: tom.GetError) {
  case error {
    tom.NotFound(key) ->
      log
      |> palabres.string("type", "Key not found")
      |> palabres.string("key", string.join(key, "/"))
    tom.WrongType(key, expected, got) ->
      log
      |> palabres.string("type", "Wrong type")
      |> palabres.string("key", string.join(key, "/"))
      |> palabres.string("got", got)
      |> palabres.string("expected", expected)
  }
}

fn log_pog_error(log: palabres.Log, error: pog.QueryError) {
  case error {
    pog.QueryTimeout -> palabres.string(log, "type", "Query timeout")
    pog.ConstraintViolated(message, constraint, details) ->
      log
      |> palabres.string("type", "Constraint violated")
      |> palabres.string("message", message)
      |> palabres.string("constraint", constraint)
      |> palabres.string("details", details)
    pog.PostgresqlError(code, name, message) ->
      log
      |> palabres.string("type", "PostgreSQL error")
      |> palabres.string("name", name)
      |> palabres.string("message", message)
      |> palabres.string("error", {
        result.unwrap(pog.error_code_name(code), code)
      })
    pog.UnexpectedArgumentCount(expected, got) ->
      log
      |> palabres.string("type", "Unexpected argument count")
      |> palabres.int("expected", expected)
      |> palabres.int("got", got)
    pog.UnexpectedArgumentType(expected, got) ->
      log
      |> palabres.string("type", "Unexpected argument type")
      |> palabres.string("expected", expected)
      |> palabres.string("got", got)
    pog.UnexpectedResultType(errors) ->
      log
      |> palabres.string("type", "Unexpected result type")
      |> log_dynamic_error(errors)
    pog.ConnectionUnavailable ->
      palabres.string(log, "type", "Connection unavailable")
  }
}
