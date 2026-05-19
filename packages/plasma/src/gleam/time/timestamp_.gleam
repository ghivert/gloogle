import gleam/json.{type Json}
import gleam/time/calendar
import gleam/time/timestamp.{type Timestamp}

pub fn encode(timestamp: Timestamp) -> Json {
  timestamp
  |> timestamp.to_rfc3339(calendar.utc_offset)
  |> json.string
}
