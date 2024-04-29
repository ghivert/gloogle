import gleam/hackney
import gleam/json
import gleam/pgo

pub type Error {
  DatabaseError(pgo.QueryError)
  FetchError(hackney.Error)
  JsonError(json.DecodeError)
  UnknownError(String)
}
