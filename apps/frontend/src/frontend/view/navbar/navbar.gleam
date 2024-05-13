import data/model.{type Model}
import data/msg
import data/search_result
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
    case model.search_results {
      search_result.Start -> h.div([], [])
      search_result.NoSearchResults | search_result.SearchResults(_, _) ->
        s.navbar_search([], [
          s.navbar_search_title([e.on_click(msg.Reset)], [
            s.search_lucy([a.src("/images/lucy.svg")]),
            h.text("Gloogle"),
          ]),
          s.search_input_wrapper([e.on_submit(msg.SubmitSearch)], [
            s.search_input([
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
