import data/model.{type Model}
import data/msg
import frontend/router
import frontend/view/navbar/styles as s
import lustre/attribute as a
import lustre/element/html as h
import lustre/event as e

fn navbar_links() {
  s.nav_links([], [
    // s.trending([], [
    //   h.text("Packages"),
    //   s.coming_soon([], [h.text(" (coming soon…)")]),
    // ]),
    s.trending([], [
      h.text("Trending"),
      s.coming_soon([], [h.text(" (coming soon…)")]),
    ]),
  ])
}

pub fn navbar(model: Model) {
  s.navbar([a.class("navbar")], [
    case model.route {
      router.Home -> h.div([], [])
      router.Search(_) ->
        s.navbar_search([], [
          s.navbar_search_title([a.href("/")], [
            s.search_lucy([a.src("/images/lucy.svg")]),
            h.text("Gloogle"),
          ]),
          s.search_input_wrapper([e.on_submit(msg.SubmitSearch)], [
            s.search_input(model.loading, [
              a.placeholder("Search for a function, or a type"),
              e.on_input(msg.UpdateInput),
              a.value(model.input),
            ]),
          ]),
        ])
    },
    navbar_links(),
  ])
}
