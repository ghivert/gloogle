import gleam/bool
import gleam/int
import gleam/list
import gleam/string
import grille_pain/internals/data/model.{type Model, Model}
import grille_pain/internals/data/msg.{
  type Msg, HideToast, ResumeToast, StopToast,
}
import grille_pain/internals/data/toast.{type Level, type Toast}
import grille_pain/internals/view/colors
import grille_pain/internals/view/progress_bar
import lustre/attribute
import lustre/element
import lustre/element/html
import lustre/event
import sketch
import sketch/size.{px}

pub fn view(model: Model) {
  let Model(toasts, _, _) = model
  element.keyed(html.div([attribute.class("grille-pain")], _), {
    use toast <- list.map(toasts)
    let id = int.to_string(toast.id)
    #(id, view_toast_wrapper(toast))
  })
}

fn view_toast_wrapper(toast: Toast) {
  let on_hide = event.on_click(HideToast(toast.id, toast.iteration))
  html.div([wrapper_position_style(toast), wrapper_dom_classes(toast)], [
    view_toast(toast, [
      html.div([text_wrapper(), on_hide], [html.text(toast.content)]),
      progress_bar.view(toast),
    ]),
  ])
}

fn view_toast(toast: Toast, children: List(element.Element(Msg))) {
  html.div(
    [
      toast_colors(toast.level),
      toast_class(),
      event.on_mouse_enter(StopToast(toast.id)),
      event.on_mouse_leave(ResumeToast(toast.id)),
    ],
    children,
  )
}

fn wrapper_position_style(toast: Toast) {
  let min_bot = int.max(0, toast.bottom)
  ["toast", bool.to_string(toast.displayed), int.to_string(min_bot)]
  |> string.join("-")
  |> sketch.dynamic([
    sketch.padding(px(12)),
    sketch.position("fixed"),
    sketch.top(px(min_bot)),
    sketch.transition("right 0.7s, top 0.7s"),
    sketch.z_index(1_000_000),
    case toast.displayed {
      True -> sketch.right(px(0))
      False ->
        sketch.right_("calc(-1 * var(--grille_pain-width, 320px) - 100px)")
    },
  ])
  |> sketch.to_lustre()
}

fn wrapper_dom_classes(toast: Toast) {
  let displayed = case toast.displayed {
    True -> "visible"
    False -> "hidden"
  }
  attribute.classes([
    #("grille_pain-toast", True),
    #("grille_pain-toast-" <> int.to_string(toast.id), True),
    #("grille_pain-toast-" <> displayed, True),
  ])
}

fn toast_class() {
  sketch.class([
    sketch.display("flex"),
    sketch.flex_direction("column"),
    // Sizes
    sketch.width_("var(--grille_pain-width, 320px)"),
    sketch.min_height_("var(--toast-min-height, 64px)"),
    sketch.max_height_("var(--toast-max-height, 800px)"),
    // Spacings
    sketch.border_radius_("var(--grille_pain-border-radius, 6px)"),
    // Colors
    sketch.box_shadow("0px 4px 12px rgba(0, 0, 0, 0.1)"),
    // Animation
    sketch.overflow("hidden"),
  ])
  |> sketch.to_lustre()
}

fn toast_colors(level: Level) {
  let #(background, text_color) = colors.from_level(level)
  let id = string.join(["grille_pain", background, text_color], "-")
  sketch.to_lustre(
    sketch.dynamic(id, [
      sketch.background(
        "var(--grille_pain-info-background, " <> background <> ")",
      ),
      sketch.color("var(--grille_pain-info-text-color, " <> text_color <> ")"),
    ]),
  )
}

fn text_wrapper() {
  sketch.class([
    sketch.display("flex"),
    sketch.align_items("center"),
    sketch.flex("1"),
    sketch.padding_("8px 16px"),
    sketch.font_size(px(12)),
  ])
  |> sketch.to_lustre()
}
