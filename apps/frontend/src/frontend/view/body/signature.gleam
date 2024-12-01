import data/search_result
import data/signature.{type Parameter, type Type, Parameter}
import frontend/view/helpers
import frontend/view/types as t
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import lustre/attribute as a
import lustre/element as el
import lustre/element/html as h

fn do_render_parameters(from: Int, to: Int, acc: List(el.Element(a))) {
  use <- bool.guard(when: from > to, return: acc)
  let assert Ok(utf_a) =
    string.to_utf_codepoints("a")
    |> list.first()
  let assert Ok(letter) =
    { string.utf_codepoint_to_int(utf_a) + from }
    |> string.utf_codepoint()
  do_render_parameters(from + 1, to, [
    t.variable(string.from_utf_codepoints([letter])),
    ..acc
  ])
}

fn render_parameters(count: Int) {
  case count {
    0 -> []
    count ->
      do_render_parameters(0, int.max(count - 1, 0), [])
      |> list.reverse()
      |> list.intersperse(h.text(", "))
      |> fn(t) { list.flatten([[h.text("(")], t, [h.text(")")]]) }
  }
}

fn view_type(type_: Type, indent: Int) -> List(el.Element(msg)) {
  case type_ {
    signature.Tuple(width, elements) -> {
      let inline = width + indent <= 80
      list.flatten([
        [helpers.idt(indent), h.text("#(")],
        case inline {
          False -> [
            helpers.newline(),
            ..{
              list.map(elements, view_type(_, indent + 2))
              |> list.intersperse([h.text(","), helpers.newline()])
              |> list.flatten()
            }
          ]
          True ->
            list.map(elements, view_type(_, 0))
            |> list.intersperse([h.text(", ")])
            |> list.flatten()
        },
        [
          bool.guard(inline, h.text(""), fn() { helpers.idt(indent) }),
          h.text(")"),
        ],
      ])
    }
    signature.Fn(width, parameters, return) -> {
      let inline = width + indent <= 80
      list.flatten([
        [helpers.idt(indent), t.keyword("fn"), h.text("(")],
        case inline {
          True ->
            list.map(parameters, view_type(_, 0))
            |> list.intersperse([h.text(", ")])
            |> list.flatten()
          False -> [
            helpers.newline(),
            ..{
              list.map(parameters, view_type(_, indent + 2))
              |> list.intersperse([h.text(","), helpers.newline()])
              |> list.flatten()
            }
          ]
        },
        bool.guard(inline, [h.text("")], fn() {
          [h.text(","), helpers.newline(), helpers.idt(indent)]
        }),
        [
          h.text(")"),
          h.text(" -> "),
          ..view_type(return, case inline {
            False if return.width > 70 -> indent
            _ -> 0
          })
        ],
      ])
    }
    signature.Variable(_, id) -> {
      [
        h.span([], {
          let assert Ok(utf_a) =
            string.to_utf_codepoints("a")
            |> list.first()
          let assert Ok(letter) =
            { string.utf_codepoint_to_int(utf_a) + id }
            |> string.utf_codepoint()
          [
            helpers.idt(indent),
            t.variable(string.from_utf_codepoints([letter])),
          ]
        }),
      ]
    }
    signature.Named(width, name, package, module, parameters, version) -> {
      let inline = width + indent <= 80
      let is_params = !list.is_empty(parameters)
      list.flatten([
        [
          helpers.idt(indent),
          case version {
            None -> t.type_(name)
            Some(version) ->
              h.a(
                [
                  a.class("named-type-button"),
                  a.target("_blank"),
                  a.rel("noreferrer"),
                  a.href(helpers.hexdocs_link(
                    package: package,
                    version: version,
                    module: module,
                    name: name,
                  )),
                ],
                [t.type_(name)],
              )
          },
          case is_params {
            True -> h.text("(")
            False -> el.none()
          },
        ],
        case inline {
          True ->
            list.map(parameters, view_type(_, 0))
            |> list.intersperse([h.text(", ")])
            |> list.flatten()
          False -> [
            helpers.newline(),
            ..{
              list.map(parameters, view_type(_, indent + 2))
              |> list.intersperse([h.text(","), helpers.newline()])
              |> list.flatten()
            }
          ]
        },
        [
          bool.guard(inline, h.text(""), fn() { helpers.idt(indent) }),
          case is_params {
            True -> h.text(")")
            False -> el.none()
          },
        ],
      ])
    }
  }
}

fn view_parameter(parameter: Parameter, indent: Int) {
  let Parameter(width, label, type_) = parameter
  list.flatten([
    case label {
      None -> [el.none()]
      Some(label) -> [helpers.idt(indent), t.label(label), h.text(": ")]
    },
    case width > 80, label {
      False, _ -> view_type(type_, 0)
      True, Some(_) -> [helpers.newline(), ..view_type(type_, indent + 2)]
      True, None -> view_type(type_, indent)
    },
  ])
}

fn view_type_constructor(constructor: signature.TypeConstructor, indent: Int) {
  let inline = constructor.params_width <= 70
  let has_params = !list.is_empty(constructor.parameters)
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
      False ->
        list.flatten([
          [helpers.newline()],
          list.map(constructor.parameters, view_parameter(_, { indent + 2 }))
            |> list.intersperse([h.text(","), helpers.newline()])
            |> list.flatten(),
          [h.text(","), helpers.newline()],
        ])
      True ->
        list.map(constructor.parameters, view_parameter(_, 0))
        |> list.intersperse([h.text(", ")])
        |> list.flatten()
    },
    case has_params, inline {
      False, _ -> []
      True, True -> [h.text(")")]
      True, False -> [helpers.idt(indent), h.text(")")]
    },
  ])
}

pub fn view_signature(item: search_result.SearchResult) -> List(el.Element(msg)) {
  case item.json_signature {
    signature.TypeDefinition(parameters, constructors) ->
      list.flatten([
        [t.keyword("type "), t.fun(item.name), ..render_parameters(parameters)],
        case constructors {
          [] -> []
          _ -> [h.text(" {"), helpers.newline()]
        },
        case constructors {
          [] -> []
          _ ->
            constructors
            |> list.map(view_type_constructor(_, 2))
            |> list.intersperse([helpers.newline()])
            |> list.flatten()
        },
        case constructors {
          [] -> []
          _ -> [helpers.newline(), h.text("}")]
        },
      ])
    signature.Constant(width, type_) ->
      list.flatten([
        [t.keyword("const "), t.fun(item.name), h.text(" = ")],
        case width > 80 {
          True -> [helpers.newline(), ..view_type(type_, 2)]
          False -> view_type(type_, 0)
        },
      ])
    signature.TypeAlias(width, parameters, alias) -> {
      list.flatten([
        [
          t.keyword("type "),
          t.type_(item.name),
          ..render_parameters(parameters)
        ],
        [h.text(" = ")],
        case width > 80 {
          True -> [helpers.newline(), ..view_type(alias, 2)]
          False -> view_type(alias, 0)
        },
      ])
    }
    signature.Function(_width, params_width, name, return, parameters) -> {
      list.flatten([
        [t.keyword("fn "), t.fun(name), h.text("(")],
        case params_width > 70 {
          True ->
            list.flatten([
              [helpers.newline(), helpers.idt(2)],
              list.map(parameters, view_parameter(_, 2))
                |> list.intersperse([
                  h.text(","),
                  helpers.newline(),
                  helpers.idt(2),
                ])
                |> list.flatten(),
              [h.text(","), helpers.newline()],
            ])
          False ->
            list.map(parameters, view_parameter(_, 0))
            |> list.intersperse([h.text(", ")])
            |> list.flatten()
        },
        [
          h.text(") -> "),
          ..case return.width + string.length(name) > 60 {
            True -> [helpers.newline(), ..view_type(return, 2)]
            False -> view_type(return, 0)
          }
        ],
      ])
    }
  }
}
