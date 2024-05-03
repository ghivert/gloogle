import gleam/bool
import gleam/iterator
import gleam/list
import gleam/option.{None, Some}
import gleam/package_interface.{
  type Parameter, type Type, type TypeConstructor, type TypeDefinition,
}
import gleam/result
import gleam/string
import ranger

fn type_definition_parameters_to_string(type_def: TypeDefinition) {
  use <- bool.guard(when: type_def.parameters == 0, return: "")
  let assert Ok(from_a) =
    ranger.create_infinite(
      validate: fn(a) { string.length(a) == 1 },
      add: fn(a: String, b: Int) {
        let assert [code] = string.to_utf_codepoints(a)
        let int_code = string.utf_codepoint_to_int(code)
        let new_int_code = int_code + b
        let assert Ok(new_code) = string.utf_codepoint(new_int_code)
        string.from_utf_codepoints([new_code])
      },
      compare: string.compare,
    )("a", 1)
  from_a
  |> iterator.take(type_def.parameters)
  |> iterator.to_list()
  |> string.join(", ")
  |> fn(s) { "(" <> s <> ")" }
}

pub fn type_definition_to_string(type_name: String, type_def: TypeDefinition) {
  let params = type_definition_parameters_to_string(type_def)
  let base = type_name <> params
  use <- bool.guard(when: type_def.constructors == [], return: base)
  base <> " {\n" <> type_constructors_to_string(type_def.constructors) <> "\n}"
}

fn type_constructors_to_string(constructors: List(TypeConstructor)) {
  constructors
  |> list.map(fn(c) {
    let params = parameters_to_string(c.parameters)
    let const_ = "  " <> c.name <> params
    case c.documentation {
      None -> const_
      Some(d) -> string.join(["  -- " <> d, const_], "\n")
    }
  })
  |> string.join("\n")
}

fn parameters_to_string(parameters: List(Parameter)) {
  use <- bool.guard(when: list.is_empty(parameters), return: "")
  parameters
  |> list.map(fn(s) {
    let label =
      s.label
      |> option.map(string.append(_, ": "))
      |> option.unwrap("")
    label <> type_to_string(s.type_)
  })
  |> string.join(", ")
  |> fn(s) { "(" <> s <> ")" }
}

fn type_to_string(type_: Type) {
  case type_ {
    package_interface.Tuple(elements) -> {
      let els =
        elements
        |> list.map(type_to_string)
        |> string.join(", ")
      "#(" <> els <> ")"
    }
    package_interface.Fn(parameters, return) -> {
      let ret = type_to_string(return)
      let params =
        parameters
        |> list.map(type_to_string)
        |> string.join(", ")
      "fn(" <> params <> ") -> " <> ret
    }
    package_interface.Variable(id) -> {
      let assert Ok(utf_a) =
        "a"
        |> string.to_utf_codepoints()
        |> list.first()
      { string.utf_codepoint_to_int(utf_a) + id }
      |> string.utf_codepoint()
      |> result.map(list.prepend([], _))
      |> result.map(string.from_utf_codepoints)
      |> result.unwrap("a")
    }
    package_interface.Named(name, package, module, parameters) -> {
      let params =
        parameters
        |> list.map(type_to_string)
        |> string.join(", ")
        |> fn(s) {
          use <- bool.guard(when: string.is_empty(s), return: s)
          "(" <> s <> ")"
        }
      case package {
        "" -> name <> params
        _ -> module <> "." <> name <> params
      }
    }
  }
}
