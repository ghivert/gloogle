import data/decoders/search_result
import data/decoders/signature.{type Parameter, type Type, Parameter}
import data/model.{type Model}
import data/msg
import frontend/footer/view as footer
import frontend/styles as s
import frontend/types as t
import gleam/bool
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import lustre/event as e

pub fn newline() {
  h.text("\n")
}

pub fn idt(indent: Int) {
  h.text(string.repeat(" ", indent))
}

pub const gloogle_description = "Gloogle can search through all public gleam packages, to help you find the function you're looking for! Enter a type or a function name to get some results."

pub fn view(model: Model) {
  h.div([s.layout()], [navbar(), body(model), footer.view()])
}

fn navbar() {
  h.div([s.navbar()], [
    h.div([], [h.text("Packages")]),
    h.div([s.trending()], [
      h.text("Trending"),
      h.span([s.coming_soon()], [h.text(" (coming soon…)")]),
    ]),
  ])
}

fn view_type(type_: Type, indent: Int) -> List(element.Element(msg.Msg)) {
  case type_ {
    signature.Tuple(width, elements) -> {
      let inline = width + indent <= 80
      list.concat([
        [idt(indent), h.text("#(")],
        case inline {
          False -> [
            newline(),
            ..{
              list.map(elements, view_type(_, indent + 2))
              |> list.intersperse([h.text(","), newline()])
              |> list.concat()
            }
          ]
          True ->
            list.map(elements, view_type(_, 0))
            |> list.intersperse([h.text(", ")])
            |> list.concat()
        },
        [bool.guard(inline, h.text(""), fn() { idt(indent) }), h.text(")")],
      ])
    }
    signature.Fn(width, parameters, return) -> {
      let inline = width + indent <= 80
      list.concat([
        [idt(indent), t.keyword("fn"), h.text("(")],
        case inline {
          True ->
            list.map(parameters, view_type(_, 0))
            |> list.intersperse([h.text(", ")])
            |> list.concat()
          False -> [
            newline(),
            ..{
              list.map(parameters, view_type(_, indent + 2))
              |> list.intersperse([h.text(","), newline()])
              |> list.concat()
            }
          ]
        },
        bool.guard(inline, [h.text("")], fn() {
          [h.text(","), newline(), idt(indent)]
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
            { string.utf_codepoint_to_int(utf_a) + id - 1 }
            |> string.utf_codepoint()
          [idt(indent), t.variable(string.from_utf_codepoints([letter]))]
        }),
      ]
    }
    signature.Named(width, name, _package, _module, parameters, _ref) -> {
      let inline = width + indent <= 80
      let is_params = !list.is_empty(parameters)
      list.concat([
        [
          idt(indent),
          t.type_(name),
          case is_params {
            True -> h.text("(")
            False -> element.none()
          },
        ],
        case inline {
          True ->
            list.map(parameters, view_type(_, 0))
            |> list.intersperse([h.text(", ")])
            |> list.concat()
          False -> [
            newline(),
            ..{
              list.map(parameters, view_type(_, indent + 2))
              |> list.intersperse([h.text(","), newline()])
              |> list.concat()
            }
          ]
        },
        [
          bool.guard(inline, h.text(""), fn() { idt(indent) }),
          case is_params {
            True -> h.text(")")
            False -> element.none()
          },
        ],
      ])
    }
  }
}

fn view_parameter(parameter: Parameter, indent: Int) {
  let Parameter(width, label, type_) = parameter
  list.concat([
    case label {
      None -> [element.none()]
      Some(label) -> [idt(indent), t.label(label), h.text(": ")]
    },
    case width > 80, label {
      False, _ -> view_type(type_, 0)
      True, Some(_) -> [newline(), ..view_type(type_, indent + 2)]
      True, None -> view_type(type_, indent)
    },
  ])
}

fn do_render_parameters(from: Int, to: Int, acc: List(element.Element(a))) {
  use <- bool.guard(when: from == to, return: acc)
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
      |> fn(t) { list.concat([[h.text("(")], t, [h.text(")")]]) }
  }
}

fn view_type_constructor(constructor: signature.TypeConstructor, indent: Int) {
  let inline = constructor.params_width <= 70
  list.concat([
    [idt(indent), t.type_(constructor.name), h.text("(")],
    case inline {
      False ->
        list.concat([
          [newline()],
          list.map(constructor.parameters, view_parameter(_, { indent + 2 }))
            |> list.intersperse([h.text(","), newline()])
            |> list.concat(),
          [h.text(","), newline()],
        ])
      True ->
        list.map(constructor.parameters, view_parameter(_, 0))
        |> list.intersperse([h.text(", ")])
        |> list.concat()
    },
    case inline {
      True -> [h.text(")")]
      False -> [idt(indent), h.text(")")]
    },
  ])
}

fn view_signature(
  item: search_result.SearchResult,
) -> List(element.Element(msg.Msg)) {
  case item.json_signature {
    signature.TypeDefinition(parameters, constructors) ->
      list.concat([
        [t.keyword("type "), t.fun(item.name), ..render_parameters(parameters)],
        case constructors {
          [] -> []
          _ -> [h.text(" {"), newline()]
        },
        case constructors {
          [] -> []
          _ ->
            constructors
            |> list.map(view_type_constructor(_, 2))
            |> list.intersperse([newline()])
            |> list.concat()
        },
        case constructors {
          [] -> []
          _ -> [newline(), h.text("}")]
        },
      ])
    signature.Constant(width, type_) ->
      list.concat([
        [t.keyword("const "), t.fun(item.name), h.text(" = ")],
        case width > 80 {
          True -> [newline(), ..view_type(type_, 2)]
          False -> view_type(type_, 0)
        },
      ])
    signature.TypeAlias(width, parameters, alias) -> {
      list.concat([
        [
          t.keyword("type "),
          t.type_(item.name),
          ..render_parameters(parameters)
        ],
        [h.text(" = ")],
        case width > 80 {
          True -> [newline(), ..view_type(alias, 2)]
          False -> view_type(alias, 0)
        },
      ])
    }
    signature.Function(_width, params_width, name, return, parameters) -> {
      list.concat([
        [t.keyword("fn "), t.fun(name), h.text("(")],
        case params_width > 70 {
          True ->
            list.concat([
              [newline()],
              list.map(parameters, view_parameter(_, 2))
                |> list.intersperse([h.text(","), newline()])
                |> list.concat(),
              [h.text(","), newline()],
            ])
          False ->
            list.map(parameters, view_parameter(_, 0))
            |> list.intersperse([h.text(", ")])
            |> list.concat()
        },
        [
          h.text(") -> "),
          ..case return.width + string.length(name) > 60 {
            True -> [newline(), ..view_type(return, 2)]
            False -> view_type(return, 0)
          }
        ],
      ])
    }
  }
}

fn view_search_results(search_results: List(search_result.SearchResult)) {
  element.fragment({
    use item <- list.map(search_results)
    h.div([s.search_result()], [
      h.div([s.search_details()], [h.text("Function")]),
      h.div([s.search_body()], [
        h.code([s.signature()], view_signature(item)),
        case item.documentation == "" {
          False ->
            h.div([s.documentation()], [
              h.div([s.documentation_title()], [h.text("Documentation")]),
              h.text(item.documentation),
            ])
          True -> element.none()
        },
      ]),
    ])
  })
}

fn view_search_input(model: Model) {
  h.form([e.on_submit(msg.SubmitSearch), s.search_wrapper()], [
    h.div([s.search_title_wrapper()], [
      h.div([s.search_title()], [
        h.img([a.src("/images/lucy.svg"), s.search_lucy()]),
        h.text("Gloogle"),
      ]),
      h.text(gloogle_description),
    ]),
    h.input([
      s.search_input(),
      a.placeholder("Search for a function, or a type"),
      e.on_input(msg.UpdateInput),
      a.value(model.input),
    ]),
    h.input([a.type_("submit"), a.value("Submit"), s.search_submit()]),
  ])
}

fn body(model: Model) {
  h.main([s.main_wrapper()], case model.search_results {
    [] -> [view_search_input(model)]
    _ -> [view_search_results(model.search_results)]
  })
}
