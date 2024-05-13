import frontend/view/footer/links.{links}
import frontend/view/footer/styles as s
import gleam/list
import lustre/attribute as a
import lustre/element/html as h

pub fn view() {
  s.footer([], [
    s.footer_links([], {
      use #(title, links) <- list.map(links)
      s.footer_section([], {
        links
        |> list.map(fn(i) { s.foot_lk([a.href(i.0)], [h.text(i.1)]) })
        |> list.prepend(s.foot_title([], [h.text(title)]))
      })
    }),
    s.footer_built([], [
      h.text("Gloogle is proudly built with 💜 in gleam for gleamlins"),
    ]),
  ])
}
