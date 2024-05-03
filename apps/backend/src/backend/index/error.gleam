import gleam/dynamic
import gleam/json
import gleam/pgo
import simplifile
import tom

pub type Error {
  DatabaseError(pgo.QueryError)
  FetchError(dynamic.Dynamic)
  JsonError(json.DecodeError)
  SimplifileError(simplifile.FileError, String)
  UnknownError(String)
  TomlError(tom.ParseError)
}
