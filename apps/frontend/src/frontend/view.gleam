import data/model.{type Model}
import frontend/colors/palette
import frontend/router
import frontend/view/body/body
import frontend/view/footer/footer
import frontend/view/navbar/navbar
import lustre/element as el
import sketch
import sketch/lustre/extra as l
import sketch/size

fn layout(attributes, children) {
  l.memo("div", attributes, children, [
    sketch.display("grid"),
    sketch.grid_template_areas(
      "\"sidebar navbar\"
       \"sidebar main\"
       \"sidebar footer\"",
    ),
    sketch.property("--a-color", palette.dark.faff_pink),
    sketch.grid_template_columns("auto 1fr"),
    sketch.grid_template_rows("auto 1fr auto"),
    sketch.min_height(size.vh(100)),
    sketch.background(palette.dark.underwater_blue),
    sketch.color(palette.dark.white),
  ])
}

pub fn view(model: Model) {
  layout([], [
    case model.route {
      router.Home -> navbar.navbar(model)
      _ -> el.none()
    },
    body.body(model),
    case model.route {
      router.Home -> footer.view()
      _ -> el.none()
    },
  ])
}
