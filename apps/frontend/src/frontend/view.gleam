import data/model.{type Model}
import frontend/colors/palette
import frontend/router
import frontend/view/body/body
import frontend/view/footer/footer
import frontend/view/navbar/navbar
import lustre/element as el
import sketch
import sketch/lustre/element as l
import sketch/media
import sketch/size.{px}

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
    sketch.media(media.max_width(px(700)), [
      sketch.grid_template_areas("\"navbar\" \"main\" \"footer\""),
      sketch.grid_template_columns("1fr"),
    ]),
  ])
}

pub fn view(model: Model) {
  layout([], [
    case model.route {
      router.Home | router.Search(_) -> navbar.navbar(model)
      _ -> el.none()
    },
    body.body(model),
    case model.route {
      router.Home -> footer.view()
      router.Search(_) -> footer.search_bar(model)
      _ -> el.none()
    },
  ])
}
