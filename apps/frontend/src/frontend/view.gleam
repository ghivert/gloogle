import data/decoders/kind
import data/decoders/search_result
import data/decoders/signature.{type Parameter, type Type, Parameter}
import data/model.{type Model}
import data/msg
import frontend/documentation
import frontend/footer/view as footer
import frontend/strings as frontend_strings
import frontend/styles as s
import frontend/types as t
import gleam/bool
import gleam/int
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

pub fn view(model: Model) {
  h.div([s.layout()], [navbar(model), body(model), footer.view()])
}

fn navbar(model: Model) {
  h.div([s.navbar()], [
    case model.search_results {
      search_result.Start -> h.div([], [])
      search_result.NoSearchResults | search_result.SearchResults(_, _) ->
        h.div([s.navbar_search()], [
          h.a([s.navbar_search_title(), e.on_click(msg.Reset)], [
            h.img([a.src("/images/lucy.svg"), s.search_lucy()]),
            h.text("Gloogle"),
          ]),
          h.form([s.search_input_wrapper(), e.on_submit(msg.SubmitSearch)], [
            h.input([
              s.search_input(),
              a.placeholder("Search for a function, or a type"),
              e.on_input(msg.UpdateInput),
              a.value(model.input),
            ]),
          ]),
        ])
    },
    h.div([s.nav_links()], [
      h.div([s.trending()], [
        h.text("Packages"),
        h.span([s.coming_soon()], [h.text(" (coming soon…)")]),
      ]),
      h.div([s.trending()], [
        h.text("Trending"),
        h.span([s.coming_soon()], [h.text(" (coming soon…)")]),
      ]),
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
            { string.utf_codepoint_to_int(utf_a) + id }
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
  let has_params = !list.is_empty(constructor.parameters)
  list.concat([
    [
      idt(indent),
      t.type_(constructor.name),
      case has_params {
        True -> h.text("(")
        False -> element.none()
      },
    ],
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
    case has_params, inline {
      False, _ -> []
      True, True -> [h.text(")")]
      True, False -> [idt(indent), h.text(")")]
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
              [newline(), idt(2)],
              list.map(parameters, view_parameter(_, 2))
                |> list.intersperse([h.text(","), newline(), idt(2)])
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
      h.div([s.search_details()], [
        h.div([], [h.text(kind.display_kind(item.kind))]),
        h.div([], [
          t.dark_white(item.package_name <> "@" <> item.version),
          t.dark_white("."),
          t.keyword(item.module_name),
          t.dark_white("#"),
          t.fun(item.name),
        ]),
      ]),
      h.div([s.search_body()], [
        h.code([s.signature()], view_signature(item)),
        case item.documentation {
          "" -> element.none()
          _ ->
            h.div([s.documentation()], [
              h.div([s.documentation_title()], [h.text("Documentation")]),
              documentation.view(item.documentation),
            ])
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
      h.text(frontend_strings.gloogle_description),
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

fn match_title(results: List(a), title: String, content: String) {
  use <- bool.guard(when: list.is_empty(results), return: element.none())
  h.div([s.matches_titles()], [
    h.div([s.matches_title()], [h.text(title)]),
    h.div([], [h.text(content)]),
  ])
}

fn empty_state(image: String, title: String, content: String) {
  h.div([s.empty_state()], [
    h.img([a.src(image), s.empty_state_lucy()]),
    h.div([s.empty_state_titles()], [
      h.div([], [h.text(title)]),
      h.div([s.empty_state_subtitle()], [h.text(content)]),
    ]),
  ])
}

fn body(model: Model) {
  h.main([s.main_wrapper()], case model.search_results {
    search_result.Start -> [view_search_input(model)]
    search_result.NoSearchResults -> [
      empty_state(
        "/images/internal_error.png",
        "Internal server error",
        frontend_strings.internal_server_error,
      ),
    ]
    search_result.SearchResults([], []) -> [
      empty_state(
        "/images/shadow_lucy.png",
        "No match found!",
        frontend_strings.retry_query,
      ),
    ]
    search_result.SearchResults(exact, others) -> [
      match_title(exact, "Exact matches", frontend_strings.exact_match),
      view_search_results(exact),
      match_title(others, "Other matches", frontend_strings.partial_match),
      view_search_results(others),
    ]
  })
}
