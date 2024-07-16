import data/model.{type Model}
import data/msg
import frontend/router
import frontend/view/navbar/styles as s
import frontend/view/search_input/search_input
import lustre/attribute as a
import lustre/event as e
import sketch/lustre/element as el

fn navbar_links() {
  s.nav_links([], [
    s.trending([], [
      el.text("Packages"),
      s.coming_soon([], [el.text(" (coming soon…)")]),
    ]),
    // s.nav_link([a.href("/trending")], [h.text("Trending")]),
  ])
}

pub fn navbar(model: Model) {
  let transparent = model.route == router.Home
  s.navbar(transparent, [a.class("navbar")], [
    case model.route {
      router.Home -> el.element("div", [], [], [])
      router.Search(_) | router.Trending ->
        s.navbar_search([], [
          s.navbar_search_title([a.href("/")], [
            s.search_lucy([a.src("/images/lucy.svg")]),
            s.title([], [el.text("Gloogle")]),
          ]),
          s.search_input_wrapper([e.on_submit(msg.SubmitSearch)], [
            search_input.view(model.loading, model.input, False),
          ]),
        ])
    },
    navbar_links(),
  ])
}
