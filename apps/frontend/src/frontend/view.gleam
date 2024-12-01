import data/model.{type Model}
import frontend/colors/palette
import frontend/router
import frontend/view/body/body
import frontend/view/footer/footer
import frontend/view/navbar/navbar
import lustre/element as el
import sketch as s
import sketch/magic
import sketch/magic/element/html as h
import sketch/media
import sketch/size.{px}

fn layout(attributes, children) {
  s.class([
    s.display("grid"),
    s.grid_template_areas(["sidebar navbar", "sidebar main", "sidebar footer"]),
    s.property("--a-color", palette.dark.faff_pink),
    s.grid_template_columns("auto 1fr"),
    s.grid_template_rows("auto 1fr auto"),
    s.min_height(size.vh(100)),
    s.media(media.max_width(px(700)), [
      s.grid_template_areas(["navbar", "main", "footer"]),
      s.grid_template_columns("1fr"),
    ]),
  ])
  |> h.div(attributes, children)
}

pub fn view(model: Model) {
  use <- magic.render([magic.node()])
  layout([], [
    navbar.navbar(model),
    body.body(model),
    case model.route {
      router.Home -> footer.view()
      router.Search(_) -> footer.search_bar(model)
      _ -> el.none()
    },
  ])
}
