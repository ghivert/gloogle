import data/decoders/search_result
import data/decoders/signature.{
  type Parameter, type Signature, type Type, Parameter,
}
import data/model.{type Model}
import data/msg
import frontend/footer/view as footer
import frontend/styles as s
import gleam/bool
import gleam/io
import gleam/list
import gleam/option
import gleam/string
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import lustre/event as e

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

fn view_type(type_: Type) {
  case type_ {
    signature.Tuple(elements) ->
      h.div(
        [s.flex()],
        list.concat([
          [h.text("#(")],
          list.map(elements, view_type)
            |> list.intersperse(h.text(", ")),
          [h.text(")")],
        ]),
      )
    signature.Fn(parameters, return) ->
      h.div(
        [s.flex()],
        list.concat([
          [h.text("fn(")],
          list.map(parameters, view_type)
            |> list.intersperse(h.text(", ")),
          [h.text(") -> "), view_type(return)],
        ]),
      )
    signature.Variable(id) ->
      h.div([], {
        let assert Ok(utf_a) =
          string.to_utf_codepoints("a")
          |> list.first()
        let assert Ok(letter) =
          { string.utf_codepoint_to_int(utf_a) + id }
          |> string.utf_codepoint()
        [h.text(string.from_utf_codepoints([letter]))]
      })
    signature.Named(name, package, module, parameters, ref) -> {
      let parameters =
        list.map(parameters, view_type)
        |> list.intersperse(h.text(", "))
      h.div(
        [s.flex()],
        list.concat([
          [h.text(name)],
          case parameters {
            [] -> [element.none()]
            _ -> [h.text("("), ..parameters]
          },
          [h.text(")")],
        ]),
      )
    }
  }
}

fn view_parameter(parameter: Parameter) {
  let Parameter(label, type_) = parameter
  let label =
    option.map(label, fn(t) { h.text(t <> ": ") })
    |> option.unwrap(element.none())
  h.div([s.flex()], [label, view_type(type_)])
}

fn render_parameters(from: Int, to: Int, acc: String) {
  use <- bool.guard(when: from == to, return: acc)
  let assert Ok(utf_a) =
    string.to_utf_codepoints("a")
    |> list.first()
  let assert Ok(letter) =
    { string.utf_codepoint_to_int(utf_a) + from }
    |> string.utf_codepoint()
  render_parameters(
    from + 1,
    to,
    acc <> ", " <> string.from_utf_codepoints([letter]),
  )
}

fn view_signature(signature: Signature) {
  case signature {
    signature.TypeDefinition(parameters, constructors) -> h.div([], [])
    signature.Constant(type_) -> view_type(type_)
    signature.TypeAlias(parameters, alias) ->
      h.div([], [h.text("(" <> render_parameters(0, parameters - 1, "") <> ")")])
    signature.Function(_, return, parameters) ->
      h.div(
        [s.flex()],
        list.concat([
          [h.text("(")],
          list.map(parameters, view_parameter)
            |> list.intersperse(h.text(", ")),
          [h.text(") -> "), view_type(return)],
        ]),
      )
  }
}

fn view_search_results(search_results: List(search_result.SearchResult)) {
  element.fragment({
    use item <- list.map(search_results)
    h.div([], [
      h.div([s.signature()], [
        h.div([], [h.text(item.name <> " : fn")]),
        h.div([], [view_signature(item.json_signature)]),
      ]),
      h.div([], [h.text(item.documentation)]),
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
