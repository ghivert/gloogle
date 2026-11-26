//// Renders a search result's signature from its IR (`signature_ir`), the one
//// structured representation jupiter stores. Line breaks are decided from the
//// width each node takes printed on one line; those widths are computed here,
//// at render time, with the formulas the legacy `signature_json` precomputed.

import data/ir.{type Declaration, type Param, type Type}
import data/type_search
import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import lustre/attribute as a
import lustre/element as el
import lustre/element/html as h
import venus/view/helpers
import venus/view/types as t

// ─── Links ───────────────────────────────────────────────────────────────────

/// What a `Named` needs to become a hexdocs link: the result's own package and
/// version (its own types), and the versions jupiter resolved for the foreign
/// packages the signature mentions (`TypeSearch.links`).
type Links {
  Links(package: String, version: String, foreign: Dict(String, String))
}

/// The hexdocs page of a nominal type, when its version is known: the result's
/// own version for its own package, the resolved one for a dependency. Prelude
/// types carry no package and get no link, nor does an unresolved dependency.
fn link(name: ir.Name, links: Links) -> Result(String, Nil) {
  use <- bool.guard(when: name.package == "", return: Error(Nil))
  use version <- result.map(case name.package == links.package {
    True -> Ok(links.version)
    False -> dict.get(links.foreign, name.package)
  })
  helpers.hexdocs_link(
    package: name.package,
    version:,
    module: name.module,
    name: name.name,
  )
}

fn view_name(name: ir.Name, links: Links) -> el.Element(msg) {
  case link(name, links) {
    Error(_) -> t.type_(name.name)
    Ok(href) ->
      h.a(
        [
          a.class("named-type-button"),
          a.target("_blank"),
          a.rel("noreferrer"),
          a.href(href),
        ],
        [t.type_(name.name)],
      )
  }
}

// ─── Widths ──────────────────────────────────────────────────────────────────

/// Columns taken by a type printed on one line.
fn type_width(type_: Type) -> Int {
  case type_ {
    ir.Var(_, name) -> string.length(name)
    ir.Hole -> 1
    ir.Tuple(elements) -> sum(elements, type_width) + separators(elements) + 3
    ir.Fn(params, return, _) ->
      sum(params, param_width) + type_width(return) + separators(params) + 8
    ir.Named(name, args) ->
      case args {
        [] -> string.length(name.name)
        _ ->
          sum(args, type_width)
          + separators(args)
          + 8
          + string.length(name.name)
      }
    ir.Record(fields) ->
      sum(fields, fn(field) {
        string.length(field.name) + 2 + type_width(field.type_)
      })
      + separators(fields)
      + 4
    ir.Union(members) ->
      sum(members, type_width) + int.max(list.length(members) - 1, 0) * 3
  }
}

/// A labelled parameter: `label: Type`, `...Type` when spread.
fn param_width(param: Param) -> Int {
  let label = case param.label {
    Some(label) -> string.length(label) + 2
    None -> 0
  }
  let spread = case param.is_spread {
    True -> 3
    False -> 0
  }
  label + spread + type_width(param.type_)
}

/// `(a, b, c)` — the parameters, their `, ` separators, the parentheses.
fn params_width(params: List(Param)) -> Int {
  sum(params, param_width) + separators(params) + 2
}

fn sum(items: List(a), width: fn(a) -> Int) -> Int {
  list.fold(items, 0, fn(acc, item) { acc + width(item) })
}

/// The `, ` between n items.
fn separators(items: List(a)) -> Int {
  int.max(list.length(items) - 1, 0) * 2
}

// ─── Types ───────────────────────────────────────────────────────────────────

/// `(a, b)` after a type's name: the declared type parameters, as letters.
fn render_parameters(count: Int) {
  case count {
    0 -> []
    count ->
      list.repeat(Nil, count)
      |> list.index_map(fn(_, index) { t.variable(letter(index)) })
      |> list.intersperse(h.text(", "))
      |> fn(t) { list.flatten([[h.text("(")], t, [h.text(")")]]) }
  }
}

fn letter(index: Int) -> String {
  let assert Ok(utf_a) = string.to_utf_codepoints("a") |> list.first
  let assert Ok(letter) =
    string.utf_codepoint(string.utf_codepoint_to_int(utf_a) + index)
  string.from_utf_codepoints([letter])
}

/// Comma-separated items, on one line or one per line indented.
fn view_items(
  items: List(a),
  inline: Bool,
  indent: Int,
  view: fn(a, Int) -> List(el.Element(msg)),
) -> List(el.Element(msg)) {
  case inline {
    True ->
      list.map(items, view(_, 0))
      |> list.intersperse([h.text(", ")])
      |> list.flatten
    False -> [
      helpers.newline(),
      ..{
        list.map(items, view(_, indent + 2))
        |> list.intersperse([h.text(","), helpers.newline()])
        |> list.flatten
      }
    ]
  }
}

fn view_type(type_: Type, indent: Int, links: Links) -> List(el.Element(msg)) {
  case type_ {
    ir.Tuple(elements) -> {
      let inline = type_width(type_) + indent <= 80
      list.flatten([
        [helpers.idt(indent), h.text("#(")],
        view_items(elements, inline, indent, fn(item, indent) {
          view_type(item, indent, links)
        }),
        [
          bool.guard(inline, h.text(""), fn() { helpers.idt(indent) }),
          h.text(")"),
        ],
      ])
    }
    ir.Fn(params, return, _) -> {
      let inline = type_width(type_) + indent <= 80
      let return_width = type_width(return)
      list.flatten([
        [helpers.idt(indent), t.keyword("fn"), h.text("(")],
        view_items(params, inline, indent, fn(item, indent) {
          view_param(item, indent, links)
        }),
        bool.guard(inline, [h.text("")], fn() {
          [h.text(","), helpers.newline(), helpers.idt(indent)]
        }),
        [
          h.text(")"),
          h.text(" -> "),
          ..view_type(
            return,
            case inline {
              False if return_width > 70 -> indent
              _ -> 0
            },
            links,
          )
        ],
      ])
    }
    ir.Var(_, name) -> [
      h.span([], [helpers.idt(indent), t.variable(name)]),
    ]
    ir.Hole -> [helpers.idt(indent), t.variable("_")]
    ir.Named(name, args) -> {
      let inline = type_width(type_) + indent <= 80
      let has_args = !list.is_empty(args)
      list.flatten([
        [
          helpers.idt(indent),
          view_name(name, links),
          case has_args {
            True -> h.text("(")
            False -> el.none()
          },
        ],
        view_items(args, inline, indent, fn(item, indent) {
          view_type(item, indent, links)
        }),
        [
          bool.guard(inline, h.text(""), fn() { helpers.idt(indent) }),
          case has_args {
            True -> h.text(")")
            False -> el.none()
          },
        ],
      ])
    }
    ir.Record(fields) -> {
      let inline = type_width(type_) + indent <= 80
      list.flatten([
        [helpers.idt(indent), h.text("{ ")],
        view_items(fields, inline, indent, fn(item, indent) {
          view_field(item, indent, links)
        }),
        [
          bool.guard(inline, h.text(" "), fn() { helpers.idt(indent) }),
          h.text("}"),
        ],
      ])
    }
    ir.Union(members) ->
      list.flatten([
        [helpers.idt(indent)],
        list.map(members, view_type(_, 0, links))
          |> list.intersperse([h.text(" | ")])
          |> list.flatten,
      ])
  }
}

fn view_field(field: ir.Field, indent: Int, links: Links) {
  [
    helpers.idt(indent),
    t.label(field.name),
    h.text(": "),
    ..view_type(field.type_, 0, links)
  ]
}

fn view_param(param: Param, indent: Int, links: Links) {
  let spread = case param.is_spread {
    True -> [h.text("...")]
    False -> []
  }
  list.flatten([
    case param.label {
      None -> [helpers.idt(indent)]
      Some(label) -> [helpers.idt(indent), t.label(label), h.text(": ")]
    },
    spread,
    case param_width(param) > 80, param.label {
      False, _ -> view_type(param.type_, 0, links)
      True, Some(_) -> [
        helpers.newline(),
        ..view_type(param.type_, indent + 2, links)
      ]
      True, None -> view_type(param.type_, 0, links)
    },
  ])
}

// ─── Declarations ────────────────────────────────────────────────────────────

fn view_constructor(constructor: ir.Constructor, indent: Int, links: Links) {
  let params = constructor.parameters
  let inline = params_width(params) <= 70
  let has_params = !list.is_empty(params)
  list.flatten([
    [
      helpers.idt(indent),
      t.type_(constructor.name),
      case has_params {
        True -> h.text("(")
        False -> el.none()
      },
    ],
    case inline {
      True ->
        view_items(params, True, indent, fn(item, indent) {
          view_param(item, indent, links)
        })
      False ->
        list.flatten([
          view_items(params, False, indent, fn(item, indent) {
            view_param(item, indent, links)
          }),
          [h.text(","), helpers.newline()],
        ])
    },
    case has_params, inline {
      False, _ -> []
      True, True -> [h.text(")")]
      True, False -> [helpers.idt(indent), h.text(")")]
    },
  ])
}

pub fn view_signature(item: type_search.TypeSearch) -> List(el.Element(msg)) {
  let links =
    Links(
      package: item.package_name,
      version: item.version,
      foreign: item.links,
    )
  view_declaration(item.type_name, item.signature_ir, links)
}

fn view_declaration(name: String, declaration: Declaration, links: Links) {
  case declaration {
    ir.TypeDefinition(parameters, constructors) ->
      list.flatten([
        [t.keyword("type "), t.fun(name), ..render_parameters(parameters)],
        case constructors {
          [] -> []
          _ ->
            list.flatten([
              [h.text(" {"), helpers.newline()],
              constructors
                |> list.map(view_constructor(_, 2, links))
                |> list.intersperse([helpers.newline()])
                |> list.flatten,
              [helpers.newline(), h.text("}")],
            ])
        },
      ])
    ir.Constant(type_) ->
      list.flatten([
        [t.keyword("const "), t.fun(name), h.text(" = ")],
        case type_width(type_) > 80 {
          True -> [helpers.newline(), ..view_type(type_, 2, links)]
          False -> view_type(type_, 0, links)
        },
      ])
    ir.TypeAlias(parameters, aliased) -> {
      let width = { parameters * 2 } + 2 + type_width(aliased) + 3
      list.flatten([
        [t.keyword("type "), t.type_(name), ..render_parameters(parameters)],
        [h.text(" = ")],
        case width > 80 {
          True -> [helpers.newline(), ..view_type(aliased, 2, links)]
          False -> view_type(aliased, 0, links)
        },
      ])
    }
    ir.Function(ir.Fn(params, return, _)) ->
      list.flatten([
        [t.keyword("fn "), t.fun(name), h.text("(")],
        case params_width(params) > 70 {
          True ->
            list.flatten([
              [helpers.newline(), helpers.idt(2)],
              list.map(params, view_param(_, 2, links))
                |> list.intersperse([
                  h.text(","),
                  helpers.newline(),
                  helpers.idt(2),
                ])
                |> list.flatten,
              [h.text(","), helpers.newline()],
            ])
          False ->
            view_items(params, True, 0, fn(item, indent) {
              view_param(item, indent, links)
            })
        },
        [
          h.text(") -> "),
          ..case type_width(return) + string.length(name) > 60 {
            True -> [helpers.newline(), ..view_type(return, 2, links)]
            False -> view_type(return, 0, links)
          }
        ],
      ])
    // A function whose stored signature is not an `Fn` cannot happen from the
    // Gleam front-end; render the type as a constant would.
    ir.Function(type_) ->
      list.flatten([
        [t.keyword("fn "), t.fun(name), h.text(": ")],
        view_type(type_, 0, links),
      ])
  }
}
