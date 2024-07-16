import data/model.{type Model}
import frontend/colors/palette
import frontend/view/body/body
import frontend/view/footer/footer
import frontend/view/navbar/navbar
import sketch
import sketch/lustre/element
import sketch/size

fn layout(attributes, children) {
  element.element("div", attributes, children, [
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
}

pub fn view(model: Model) {
  layout([], [navbar.navbar(model), body.body(model), footer.view()])
}
