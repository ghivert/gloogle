import data/model.{type Model}
import data/msg
import frontend/footer/view as footer
import frontend/styles as s
import gleam/dict
import gleam/dynamic
import gleam/io
import gleam/json
import gleam/list
import gleam/result
import lustre/attribute as a
import lustre/element
import lustre/element/html as h
import lustre/event as e

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

fn body(model: Model) {
  h.main([s.main_wrapper()], [
    h.form([e.on_submit(msg.SubmitSearch), s.search_wrapper()], [
      h.div([s.search_title_wrapper()], [
        h.div([s.search_title()], [
          h.img([a.src("/images/lucy.svg"), s.search_lucy()]),
          h.text("Gloogle"),
        ]),
        h.text(
          "Gloogle can search through all public gleam packages, to help you find the function you're looking for! Enter a type or a function name to get some results.",
        ),
      ]),
      h.input([
        s.search_input(),
        a.placeholder("Search for a function, or a type"),
        e.on_input(msg.UpdateInput),
        a.value(model.input),
      ]),
      h.input([a.type_("submit"), a.value("Submit"), s.search_submit()]),
    ]),
  ])
}
