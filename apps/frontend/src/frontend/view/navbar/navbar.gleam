import data/model.{type Model}
import frontend/router
import frontend/view/navbar/styles as s
import lustre/attribute as a
import lustre/element/html as h

fn navbar_links() {
  s.nav_links([], [
    s.nav_link([a.href("/packages")], [h.text("Packages")]),
    s.nav_link([a.href("/analytics")], [h.text("Analytics")]),
  ])
}

pub fn navbar(model: Model) {
  let transparent = model.route == router.Home
  s.navbar(transparent, [a.class("navbar")], [
    case model.route {
      router.Home -> navbar_links()
      router.Search(_) | router.Trending | router.Analytics | router.Packages ->
        s.navbar_search([], [
          s.navbar_search_title([a.href("/")], [
            s.search_lucy(40, [a.src("/images/lucy.svg")]),
            s.title([], [h.text("Gloogle")]),
          ]),
        ])
    },
  ])
}
