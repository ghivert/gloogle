import api/hex
import data/analytics
import data/ir
import data/ir/parse
import data/ir/unify
import data/package
import data/type_search
import gleam/bool
import gleam/function_
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/pair
import gleam/result
import gleam/string
import jupiter/context.{type Context}
import jupiter/links
import jupiter/loss.{type Loss}
import jupiter/postgres/queries
import palabres
import pog
import tasks/hex as syncing
import wisp.{type Request}

const module = "jupiter/router/handlers"

pub fn analytics(_req: Request, ctx: Context) {
  select_all_analytics(ctx)
  |> result.map(fn(content) {
    content
    |> analytics.encode
    |> json.to_string
    |> wisp.json_response(200)
  })
  |> result.map_error(function_.tap(_, loss.log))
  |> result.unwrap(wisp.internal_server_error())
}

/// `GET /search?q=…[&broad=true]`. A query that is structurally a type —
/// `fn(List(a)) -> Bool`, `List(Int)`, `#(a, b)` — is answered by the type tier
/// alone; the text tiers would only grind trigram and tsvector searches over a
/// string shaped like code (seconds, then timeouts) to return noise. `broad=true`
/// is the "widen the search" switch: every tier runs regardless, and the type
/// tier looks at every release instead of each package's latest one (a
/// declaration removed from a package's latest version is found again, at the
/// last version that had it). A bare identifier (`Connection`, `is_empty`) is
/// not structural and always gets every tier.
pub fn search(req: Request, ctx: Context) {
  let params = wisp.get_query(req)
  let broad =
    list.key_find(params, "broad")
    |> result.map(fn(value) { value == "true" || value == "1" })
    |> result.unwrap(False)
  list.key_find(params, "q")
  |> result.replace_error(loss.EmptyError)
  |> result.map(fn(query) { do_search(query, broad, ctx) })
  |> result.unwrap(json.object([#("error", json.string("internal"))]))
  |> json.to_string
  |> wisp.json_response(200)
}

pub fn packages(_req: Request, ctx: Context) {
  queries.select_package_by_updated_at(ctx.db)
  |> result.unwrap([])
  |> json.array(package.encode)
  |> json.to_string
  |> wisp.json_response(200)
}

pub fn trendings(req: Request, ctx: Context) {
  wisp.get_query(req)
  |> list.find(fn(item) { item.0 == "page" })
  |> result.try(fn(item) { int.parse(item.1) })
  |> result.try_recover(fn(_) { Ok(0) })
  |> result.unwrap(0)
  |> queries.select_package_by_popularity(ctx.db, _)
  |> result.map(fn(content) {
    content
    |> json.array(package.encode)
    |> json.to_string
    |> wisp.json_response(200)
  })
  |> result.unwrap(wisp.internal_server_error())
}

pub fn package_update(_req: Request, ctx: Context, name: String) {
  do_update_package(ctx, name)
  |> loss.dismiss
  wisp.json_response("{}", 200)
}

fn do_update_package(ctx: Context, name: String) {
  let found_package = hex.get_package(name, ctx.hex_api_key)
  use package <- result.try(found_package)
  syncing.sync_package(ctx, package)
}

fn select_all_analytics(ctx: Context) {
  use timeseries <- result.try(queries.get_timeseries_count(ctx.db))
  use total_searches <- result.try(queries.get_total_searches(ctx.db))
  use signatures <- result.try(queries.get_total_signatures(ctx.db))
  use packages <- result.try(queries.get_total_packages(ctx.db))
  use #(ranked, popular) <- result.try({
    queries.select_more_popular_packages(ctx.db)
  })
  let total_searches = list.first(total_searches) |> result.unwrap(0)
  let total_signatures = list.first(signatures) |> result.unwrap(0)
  let total_indexed = list.first(packages) |> result.unwrap(0)
  Ok(analytics.Analytics(
    timeseries:,
    total_searches:,
    total_signatures:,
    total_indexed:,
    ranked:,
    popular:,
  ))
}

fn do_search(query: String, broad: Bool, ctx: Context) {
  palabres.notice("Search")
  |> palabres.string("query", query)
  |> palabres.bool("broad", broad)
  |> palabres.at(module:, function: "do_search")
  |> palabres.log
  queries.upsert_search_analytics(ctx.db, query)
  |> loss.dismiss

  let #(exact_type_searches, structural) = exec_type_search(ctx, query, broad)
  // A structural type query stops here unless the caller asked to widen.
  let narrowed = structural && !broad
  let exec_search = fn(ctx, query, run, previous) {
    case narrowed {
      True -> []
      False -> exec_search(ctx, query, run, previous)
    }
  }
  // The three content tiers (trigram, two tsvectors) need at least one word of
  // three characters to be selective; `fn(a) -> a` or `io` would make them scan
  // most of the table for noise (see `searchable`).
  let exec_content_search = fn(ctx, query, run, previous) {
    case searchable(query) {
      True -> exec_search(ctx, query, run, previous)
      False -> []
    }
  }

  let exact_name_matches =
    exact_type_searches
    |> exec_search(ctx, query, queries.name_search, _)

  let exact_module_and_name_matches =
    [exact_type_searches, exact_name_matches]
    |> list.flatten
    |> exec_search(ctx, query, queries.module_and_name_search, _)

  let exact_matches =
    list.flatten([exact_name_matches, exact_module_and_name_matches])

  let matches =
    [exact_matches, exact_type_searches]
    |> list.flatten
    |> exec_content_search(ctx, query, queries.content_search, _)

  let signature_searches =
    [exact_matches, exact_type_searches, matches]
    |> list.flatten
    |> exec_content_search(ctx, query, queries.signature_search, _)

  let documentation_searches =
    [exact_matches, exact_type_searches, matches, signature_searches]
    |> list.flatten
    |> exec_content_search(ctx, query, queries.documentation_search, _)

  let module_searches =
    [
      exact_matches,
      exact_type_searches,
      matches,
      signature_searches,
      documentation_searches,
    ]
    |> list.flatten
    |> exec_search(ctx, query, queries.module_search, _)

  // Resolve the type links of every tier in one batch, once the tiers have
  // deduplicated against each other (`links` is not part of that equality).
  let linked = fn(items) {
    json.array(links.attach(ctx.db, items), type_search.encode)
  }
  json.object([
    #("narrowed", json.bool(narrowed)),
    #("exact-type-matches", linked(exact_type_searches)),
    #("exact-matches", linked(exact_matches)),
    #("matches", linked(matches)),
    #("searches", linked(signature_searches)),
    #("docs-searches", linked(documentation_searches)),
    #("module-searches", linked(module_searches)),
  ])
}

/// The type-search tier: parse the query as a Gleam type, then run the two-phase
/// fingerprint + verify pipeline. `parse` fails on anything that is not a type
/// (multi-word text queries, malformed input) — those simply skip this tier and
/// fall through to the name/content tiers below. The second element says whether
/// the query was *structurally* a type (see `structural`), which is what lets
/// `do_search` skip the text tiers. Phase 1 reads each package's latest release
/// unless the caller asked for a broad search, which reads every release.
fn exec_type_search(
  ctx: Context,
  query: String,
  broad: Bool,
) -> #(List(type_search.TypeSearch), Bool) {
  case parse.parse(query) {
    Error(_) -> #([], False)
    Ok(type_) -> {
      let structural = structural(type_)
      let #(arity, type_names) = ir.fingerprint_type(type_)
      // A bare variable or hole (`a`, `_`) has no structural signal — arity 0 and
      // no names — so its fingerprint matches nearly everything. That is a text
      // query, not a type query; leave it to the other tiers.
      use <- bool.guard(
        when: arity == 0 && list.is_empty(type_names),
        return: #([], False),
      )
      let subset_keys = queries.subset_keys(type_names)
      let scope = case broad {
        True -> queries.EveryRelease
        False -> queries.LatestRelease
      }
      // A closed query — no hole, no label — has a normal form, and its exact
      // matches are a btree lookup: guaranteed complete, where phase 1 keeps
      // the hundred candidates closest by distance and best ranked.
      let exact = case closed(type_) {
        False -> []
        True ->
          queries.canonical_matches(ctx.db, ir.to_canonical(type_), scope)
          |> result.map_error(function_.tap(_, loss.log))
          |> result.unwrap([])
      }
      queries.fingerprint_candidates(
        ctx.db,
        arity,
        type_names,
        subset_keys,
        scope,
      )
      |> result.map_error(function_.tap(_, loss.log))
      |> result.unwrap([])
      |> list.filter(fn(item) { !list.contains(exact, item) })
      |> list.append(exact, _)
      |> list.filter_map(fn(item) {
        unify.unify_declaration(type_, item.signature_ir)
        |> result.map(fn(match) { #(item, match) })
      })
      |> list.sort(fn(a, b) { unify.compare(a.1, b.1) })
      |> list.map(pair.first)
      |> list.take(20)
      |> pair.new(structural)
    }
  }
}

/// Is a parsed query unmistakably a type, rather than a word that happens to
/// parse as one? A function, a tuple, a parameterised type (`List(Int)`), a
/// record or a union carry type syntax a user does not type by accident. A bare
/// name (`Connection`), variable (`a`) or hole (`_`) does not: it may well be a
/// search by name, so the text tiers must still run for it.
pub fn structural(type_: ir.Type) -> Bool {
  case type_ {
    ir.Fn(..) | ir.Tuple(..) | ir.Record(..) | ir.Union(..) -> True
    ir.Named(args: [_, ..], ..) -> True
    ir.Named(args: [], ..) | ir.Var(..) | ir.Hole -> False
  }
}

/// Does the query carry at least one word of three characters or more? Below
/// that, the content tiers degenerate: `pg_trgm` extracts no trigram from a
/// `LIKE '%io%'` pattern and the GIN returns the whole table, and the English
/// tsvector of `fn(a) -> a` is the single token `fn`, present in 377k rows.
/// Measured on the local snapshot: `content_search` on `fn(a) -> a` 750 ms,
/// `fn(a, b) -> b` 380 ms, for results that are pure noise. A word is a run of
/// letters, digits or underscores — `is_empty` counts, `fn(a, b)` does not.
pub fn searchable(query: String) -> Bool {
  query
  |> string.to_graphemes
  |> list.fold_until(0, fn(run, grapheme) {
    let run = case is_word_grapheme(grapheme) {
      True -> run + 1
      False -> 0
    }
    case run >= 3 {
      True -> list.Stop(run)
      False -> list.Continue(run)
    }
  })
  |> fn(run) { run >= 3 }
}

fn is_word_grapheme(grapheme: String) -> Bool {
  case grapheme {
    "_" -> True
    _ ->
      case string.to_utf_codepoints(grapheme) {
        [codepoint] -> {
          let c = string.utf_codepoint_to_int(codepoint)
          { c >= 48 && c <= 57 }
          || { c >= 65 && c <= 90 }
          || { c >= 97 && c <= 122 }
        }
        _ -> False
      }
  }
}

/// Has the query a normal form the exact lookup can use? A `Hole` matches
/// anything, so it has no single spelling; a label the user wrote must match the
/// candidate's, which the normal form drops. Everything else — variables
/// included, since they are alpha-renamed on both sides — is closed.
pub fn closed(type_: ir.Type) -> Bool {
  case type_ {
    ir.Hole -> False
    ir.Var(..) -> True
    ir.Named(args:, ..) -> list.all(args, closed)
    ir.Tuple(elements:) -> list.all(elements, closed)
    ir.Union(members:) -> list.all(members, closed)
    ir.Record(fields:) -> list.all(fields, fn(field) { closed(field.type_) })
    ir.Fn(params:, return:, ..) ->
      closed(return)
      && list.all(params, fn(param) {
        option.is_none(param.label) && closed(param.type_)
      })
  }
}

fn exec_search(
  ctx: Context,
  query: String,
  run: fn(pog.Connection, String) -> Loss(List(type_search.TypeSearch)),
  previous: List(type_search.TypeSearch),
) -> List(type_search.TypeSearch) {
  run(ctx.db, query)
  |> result.map_error(function_.tap(_, loss.log))
  |> result.unwrap([])
  |> list.filter(fn(i) { !list.contains(previous, i) })
}
