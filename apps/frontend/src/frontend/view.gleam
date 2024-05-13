import data/model.{type Model}
import frontend/colors/palette
import frontend/view/body/body
import frontend/view/footer/footer
import frontend/view/navbar/navbar
import gleam/list
import lustre/attribute.{type Attribute}
import lustre/element.{type Element}
import sketch
import sketch/size

fn layout(attributes: List(Attribute(msg)), children: List(Element(msg))) {
  sketch.class([
    sketch.display("grid"),
    sketch.grid_template_areas(
      "\"navbar\"
       \"main\"
       \"footer\"",
    ),
    sketch.property("--a-color", palette.dark.faff_pink),
    sketch.grid_template_rows("auto 1fr auto"),
    sketch.min_height(size.vh(100)),
    sketch.background(palette.dark.underwater_blue),
    sketch.color(palette.dark.white),
  ])
  |> sketch.memo()
  |> sketch.to_lustre()
  |> list.prepend(attributes, _)
  |> element.element("div", _, children)
}

pub fn view(model: Model) {
  layout([], [navbar.navbar(model), body.body(model), footer.view()])
}
