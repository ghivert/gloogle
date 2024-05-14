import gleam/bool
import gleam/iterator
import gleam/list
import gleam/option
import gleam/package_interface.{
  type Constant, type Function, type Parameter, type Type, type TypeAlias,
  type TypeConstructor, type TypeDefinition,
}
import gleam/result
import gleam/string
import ranger

fn int_parameters_to_string(parameters: Int) {
  use <- bool.guard(when: parameters == 0, return: "")
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
  |> iterator.take(parameters)
  |> iterator.to_list()
  |> string.join(", ")
  |> fn(s) { "(" <> s <> ")" }
}

pub fn type_definition_to_string(type_name: String, type_def: TypeDefinition) {
  let params = int_parameters_to_string(type_def.parameters)
  let base = type_name <> params
  use <- bool.guard(when: type_def.constructors == [], return: base)
  base <> " {\n" <> type_constructors_to_string(type_def.constructors) <> "\n}"
}

fn type_constructors_to_string(constructors: List(TypeConstructor)) {
  constructors
  |> list.map(fn(c) {
    let params = parameters_to_string(c.parameters)
    let const_ = "  " <> c.name <> params
    c.documentation
    |> option.map(fn(d) { string.join(["  -- " <> d, const_], "\n") })
    |> option.unwrap(const_)
  })
  |> string.join("\n")
}

fn parameters_to_string(parameters: List(Parameter)) {
  use <- bool.guard(when: list.is_empty(parameters), return: "")
  parameters
  |> list.map(fn(s) {
    s.label
    |> option.map(string.append(_, ": "))
    |> option.unwrap("")
    |> string.append(type_to_string(s.type_))
  })
  |> string.join(", ")
  |> fn(s) { "(" <> s <> ")" }
}

fn type_to_string(type_: Type) {
  case type_ {
    package_interface.Tuple(elements) ->
      elements
      |> list.map(type_to_string)
      |> string.join(", ")
      |> fn(s) { "#(" <> s <> ")" }
    package_interface.Fn(parameters, return) -> {
      let ret = type_to_string(return)
      parameters
      |> list.map(type_to_string)
      |> string.join(", ")
      |> fn(s) { "fn(" <> s <> ") -> " <> ret }
    }
    package_interface.Variable(id) -> {
      let assert Ok(utf_a) =
        string.to_utf_codepoints("a")
        |> list.first()
      { string.utf_codepoint_to_int(utf_a) + id }
      |> string.utf_codepoint()
      |> result.map(list.prepend([], _))
      |> result.map(string.from_utf_codepoints)
      |> result.unwrap("a")
    }
    package_interface.Named(name, package, module, parameters) -> {
      parameters
      |> list.map(type_to_string)
      |> string.join(", ")
      |> fn(s) {
        use <- bool.guard(when: string.is_empty(s), return: s)
        "(" <> s <> ")"
      }
      |> fn(params) {
        case package {
          "" -> name <> params
          _ -> module <> "." <> name <> params
        }
      }
    }
  }
}

pub fn type_alias_to_string(type_name: String, type_alias: TypeAlias) -> String {
  let params = int_parameters_to_string(type_alias.parameters)
  type_name <> params <> " = " <> type_to_string(type_alias.alias)
}

pub fn constant_to_string(constant_name: String, constant: Constant) -> String {
  constant_name <> " = " <> type_to_string(constant.type_)
}

pub fn function_to_string(function_name: String, function: Function) -> String {
  let params = parameters_to_string(function.parameters)
  function_name <> params <> " -> " <> type_to_string(function.return)
}
