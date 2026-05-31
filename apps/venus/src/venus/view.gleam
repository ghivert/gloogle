import bright
import data/model.{type Model}
import lustre/element as el
import sketch
import sketch/css
import sketch/css/length.{px}
import sketch/css/media
import sketch/lustre as sl
import sketch/lustre/element/html as h
import venus/colors/palette
import venus/router
import venus/view/body/body
import venus/view/footer/footer
import venus/view/navbar/navbar

fn layout(attributes, children) {
  css.class([
    css.display("grid"),
    css.grid_template_areas(["sidebar navbar", "sidebar main", "sidebar footer"]),
    css.property("--a-color", palette.dark.faff_pink),
    css.grid_template_columns("auto 1fr"),
    css.grid_template_rows("auto 1fr auto"),
    css.min_height(length.vh(100)),
    css.media(media.max_width(px(700)), [
      css.grid_template_areas(["navbar", "main", "footer"]),
      css.grid_template_columns("1fr"),
    ]),
  ])
  |> h.div(attributes, children)
}

pub fn view(model: Model, stylesheet: sketch.StyleSheet) {
  use <- sl.render(stylesheet, [sl.node()])
  let state = bright.state(model)
  layout([], [
    navbar.navbar(model),
    body.body(model),
    case state.route {
      router.Home -> footer.view()
      router.Search(_) -> footer.search_bar(model)
      _ -> el.none()
    },
  ])
}
