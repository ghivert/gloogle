import gleam/list
import gleam/option.{type Option}
import gleam/pair
import gleam/string
import lustre/element.{type Element}
import lustre/element/html as h
import lustre/event
import tardis/internals/data.{type Data}
import tardis/internals/data/debugger.{type Debugger}
import tardis/internals/data/msg.{type Msg}
import tardis/internals/data/step.{type Step, Step}
import tardis/internals/styles as s

pub fn view_model(opened: Bool, debugger_: String, model: Debugger) {
  let selected = model.selected_step
  case opened {
    False -> element.none()
    True ->
      element.keyed(h.div([s.body()], _), {
        model.steps
        |> list.take(100)
        |> list.map(fn(i) { #(i.index, view_step(debugger_, selected, i)) })
        |> list.prepend(#("header", view_grid_header(opened, model)))
      })
  }
}

fn view_step(debugger_: String, selected_step: Option(String), item: Step) {
  let Step(index, model, msg) = item
  let class = case option.unwrap(selected_step, "") == index {
    True -> s.selected_details()
    False -> s.details()
  }
  h.div([class, event.on_click(msg.BackToStep(debugger_, item))], [
    h.div([s.step_index()], [h.text(index)]),
    h.div([s.step_msg()], view_data(data.inspect(msg), 0, "")),
    h.div([s.step_model()], view_data(data.inspect(model), 0, "")),
  ])
}

fn view_data_line(indent: Int, prefix: String, text: String, color: String) {
  let idt = string.repeat(" ", times: indent)
  let text_color = s.text_color(color)
  case string.length(prefix) {
    0 -> h.div([text_color], [h.text(idt <> text)])
    _ ->
      h.div([s.flex()], [
        h.div([s.keyword_color()], [h.text(idt <> prefix)]),
        h.div([text_color], [h.text(text)]),
      ])
  }
}

fn select_grid_header_class(opened: Bool, model: Debugger) {
  case opened, model.count {
    False, _ | True, 1 -> s.grid_header()
    True, _ -> s.bordered_grid_header()
  }
}

fn view_grid_header(opened: Bool, model: Debugger) {
  h.div([select_grid_header_class(opened, model)], [
    h.div([s.subgrid_header()], [h.text("Step")]),
    h.div([s.subgrid_header()], [h.text("Msg")]),
    h.div([s.subgrid_header()], [h.text("Model")]),
  ])
}

fn view_data(data: Data, indent i: Int, prefix p: String) -> List(Element(Msg)) {
  case data {
    data.DataNil -> [view_data_line(i, p, "Nil", "var(--nil)")]
    data.DataBool(v) -> [view_data_line(i, p, v, "var(--bool)")]
    data.DataConstant(v) -> [view_data_line(i, p, v, "var(--constant)")]
    data.DataBitArray(v) -> [view_data_line(i, p, v, "var(--bit-array)")]
    data.DataUtfCodepoint(v) -> [view_data_line(i, p, v, "var(--utfcodepoint)")]
    data.DataString(v) -> [view_data_line(i, p, v, "var(--string)")]
    data.DataNumber(v) -> [view_data_line(i, p, v, "var(--number)")]
    data.DataRegex(v) -> [view_data_line(i, p, v, "var(--regex)")]
    data.DataDate(v) -> [view_data_line(i, p, v, "var(--date)")]
    data.DataFunction(v) -> [view_data_line(i, p, v, "var(--function)")]
    data.DataTuple(vs) -> view_data_tuple(vs, p, i)
    data.DataList(vs) -> view_data_list(vs, p, i)
    data.DataCustomType(name, vs) -> view_data_custom_type(name, vs, p, i)
    data.DataDict(vs) -> view_data_dict(vs, p, i)
    data.DataSet(vs) -> view_data_set(vs, p, i)
    data.DataObject(name, vs) -> view_data_object(name, vs, p, i)
  }
}

fn count_data(data: Data) {
  case data {
    data.DataNil -> 1
    data.DataBool(_) -> 1
    data.DataConstant(_) -> 1
    data.DataBitArray(_) -> 1
    data.DataUtfCodepoint(_) -> 1
    data.DataString(_) -> 1
    data.DataNumber(_) -> 1
    data.DataRegex(_) -> 1
    data.DataDate(_) -> 1
    data.DataFunction(_) -> 1
    data.DataTuple(vs) ->
      list.map(vs, count_data)
      |> list.fold(2, fn(acc, val) { acc + val })
    data.DataList(vs) ->
      list.map(vs, count_data)
      |> list.fold(2, fn(acc, val) { acc + val })
    data.DataCustomType(_, vs) ->
      list.map(vs, fn(d) { count_data(pair.second(d)) })
      |> list.fold(2, fn(acc, val) { acc + val })
    data.DataDict(vs) ->
      list.map(vs, fn(d) { count_data(pair.second(d)) })
      |> list.fold(2, fn(acc, val) { acc + val })
    data.DataSet(vs) ->
      list.map(vs, fn(d) { count_data(d) })
      |> list.fold(2, fn(acc, val) { acc + val })
    data.DataObject(_, vs) ->
      list.map(vs, fn(d) { count_data(pair.second(d)) })
      |> list.fold(2, fn(acc, val) { acc + val })
  }
}

fn view_data_tuple(values: List(Data), prefix p: String, indent i: Int) {
  list.concat([
    [view_data_line(i, p, "#(", "var(--editor-fg)")],
    list.flat_map(values, view_data(_, i + 2, "")),
    [view_data_line(i, p, ")", "var(--editor-fg)")],
  ])
}

fn view_data_list(values: List(Data), prefix p: String, indent i: Int) {
  let open_list = view_data_line(i, p, "[", "var(--editor-fg)")
  let close_list = fn(idt) { view_data_line(idt, "", "]", "var(--editor-fg)") }
  case list.is_empty(values) {
    True -> [h.div([s.flex()], [open_list, close_list(0)])]
    False ->
      list.concat([
        [open_list],
        list.flat_map(values, view_data(_, i + 2, "")),
        [close_list(i)],
      ])
  }
}

fn display_parenthesis(should_display, p) {
  case should_display {
    True -> p
    False -> ""
  }
}

fn view_data_custom_type(
  name: String,
  values: List(#(Option(String), Data)),
  prefix p: String,
  indent i: Int,
) {
  let open_type = fn(display_paren) {
    let paren = display_parenthesis(display_paren, "(")
    view_data_line(i, p, name <> paren, "var(--custom-type)")
  }
  let close_type = fn(idt, display_paren) {
    let paren = display_parenthesis(display_paren, ")")
    view_data_line(idt, "", paren, "var(--custom-type)")
  }
  let body_type = fn(inline) {
    let #(f, e) = case inline {
      False -> #(0, 0)
      True -> #(i, i + 2)
    }
    list.concat([
      [open_type(True)],
      list.flat_map(values, fn(data) {
        let prefix = option.unwrap(pair.first(data), "")
        view_data(pair.second(data), e, prefix)
      }),
      [close_type(f, True)],
    ])
  }
  case values {
    [_, _, ..] -> body_type(True)
    [_, ..] -> {
      let v =
        list.fold(values, 0, fn(acc, d) {
          case acc > 2 {
            True -> acc
            False -> {
              let data = count_data(pair.second(d))
              data + acc
            }
          }
        })
      case v > 2 {
        True -> body_type(True)
        False -> [h.div([s.flex()], body_type(False))]
      }
    }
    [] -> [h.div([s.flex()], [open_type(False), close_type(0, False)])]
  }
}

fn view_data_dict(values: List(#(Data, Data)), prefix p: String, indent i: Int) {
  list.concat([
    [view_data_line(i, p, "//js dict.from_list([", "var(--editor-fg)")],
    list.flat_map(values, fn(data) {
      [
        h.div(
          [s.flex()],
          list.concat([
            view_data(pair.first(data), i + 2, "#("),
            view_data(pair.second(data), 0, ", "),
            [h.div([s.text_color("var(--bool)")], [h.text(")")])],
            [h.text(",")],
          ]),
        ),
      ]
    }),
    [view_data_line(i, "", "])", "var(--editor-fg)")],
  ])
}

fn view_data_set(vs: List(Data), prefix p: String, indent i: Int) {
  list.concat([
    [view_data_line(i, p, "//js Set(", "var(--editor-fg)")],
    list.flat_map(vs, view_data(_, i + 2, "")),
    [view_data_line(i, p, ")", "var(--editor-fg)")],
  ])
}

fn view_data_object(
  name: String,
  vs: List(#(Data, Data)),
  prefix p: String,
  indent i: Int,
) {
  list.concat([
    [view_data_line(i, p, name <> " {", "var(--editor-fg)")],
    list.flat_map(vs, fn(data) { view_data(pair.second(data), i + 2, "") }),
    [view_data_line(i, p, "}", "var(--editor-fg)")],
  ])
}
