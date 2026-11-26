import data/ir
import data/ir/from_gleam
import data/ir/parse
import gleam/list
import gleam/option.{None}
import gleam/package_interface as pi
import gleeunit
import jupiter/context/environment
import jupiter/context/postgres
import jupiter/postgres/queries
import jupiter/router/handlers
import pog

pub fn main() {
  gleeunit.main()
}

// `Result(a, Int)` from `gleam/result`, with a prelude `Int`. Checks package +
// qualifier extraction from the module, prelude staying unqualified, `Variable`
// → `Var` (pi ids preserved, alpha-renaming is a later pass), and nesting.
pub fn lower_type_test() {
  let pi_type =
    pi.Named("Result", "gleam_stdlib", "gleam/result", [
      pi.Variable(0),
      pi.Named("Int", "", "gleam", []),
    ])
  let expected =
    ir.Named(ir.Name("gleam_stdlib", "gleam/result", "Result", None), [
      ir.Var(0, "a"),
      ir.Named(ir.Name("", "", "Int", None), []),
    ])
  assert from_gleam.type_(pi_type) == expected
}

// Phase-1 subset keys: every proper subset of the (sorted) query names as a
// canonical `,`-joined key — the empty set in, the full set out (`@>` covers
// it), each key in sorted order. 2^3 - 1 = 7 keys for three names.
pub fn subset_keys_test() {
  let keys = queries.subset_keys(["Bool", "Int", "List"])
  assert list.length(keys) == 7
  assert list.contains(keys, "")
  assert list.contains(keys, "Bool,List")
  assert list.contains(keys, "Int,List")
  assert !list.contains(keys, "Bool,Int,List")
  assert queries.subset_keys(["List"]) == [""]
  // A nameless query keeps only the empty key (its `@>` branch is off).
  assert queries.subset_keys([]) == [""]
  // Beyond six names the enumeration is skipped.
  assert queries.subset_keys(["A", "B", "C", "D", "E", "F", "G"]) == []
}

// A query is structurally a type when it carries type syntax a user does not
// type by accident; a bare identifier is not, so it keeps the text tiers.
pub fn structural_query_test() {
  let structural = fn(query) {
    let assert Ok(type_) = parse.parse(query)
    handlers.structural(type_)
  }
  assert structural("fn(List(a), fn(a) -> b) -> List(b)")
  assert structural("List(Int)")
  assert structural("#(a, b)")
  assert !structural("Connection")
  assert !structural("a")
  assert !structural("_")
}

// The content tiers (trigram, tsvectors) only run on a query carrying a word
// of three characters or more: shorter patterns make the GIN return the whole
// table. A word is a run of letters, digits or underscores.
pub fn searchable_query_test() {
  assert handlers.searchable("Error")
  assert handlers.searchable("is_empty")
  assert handlers.searchable("fn(List(a)) -> Bool")
  assert handlers.searchable("a map")
  assert !handlers.searchable("fn(a) -> a")
  assert !handlers.searchable("fn(a, b) -> b")
  assert !handlers.searchable("io")
  assert !handlers.searchable("")
}

// A query is closed — eligible for the exact lookup by normal form — when it
// has no hole and no label; variables are fine, they are alpha-renamed.
pub fn closed_query_test() {
  let closed = fn(query) {
    let assert Ok(type_) = parse.parse(query)
    handlers.closed(type_)
  }
  assert closed("fn(List(a), fn(a) -> b) -> List(b)")
  assert closed("List(Int)")
  assert !closed("fn(List(_)) -> Bool")
  assert !closed("fn(with: Int) -> Bool")
  assert !closed("_")
}

// `sslmode` in DATABASE_URL wins, with libpq's meaning; without it, no TLS in
// development and verified TLS in production. An unknown mode falls back to
// the default (pog.url_config rejects it anyway).
pub fn postgres_ssl_test() {
  let url = fn(query) { "postgres://u:p@host:5432/db" <> query }
  let prod = environment.Production
  let dev = environment.Development
  assert postgres.ssl(url(""), prod) == pog.SslVerified
  assert postgres.ssl(url(""), dev) == pog.SslDisabled
  assert postgres.ssl(url("?sslmode=require"), prod) == pog.SslUnverified
  assert postgres.ssl(url("?sslmode=disable"), prod) == pog.SslDisabled
  assert postgres.ssl(url("?sslmode=verify-full"), dev) == pog.SslVerified
  assert postgres.ssl(url("?sslmode=verify-ca"), dev) == pog.SslVerified
  assert postgres.ssl(url("?connect_timeout=5&sslmode=require"), dev)
    == pog.SslUnverified
  assert postgres.ssl(url("?sslmode=bogus"), prod) == pog.SslVerified
}
