import frontend/footer/links.{links}
import frontend/footer/styles as s
import gleam/list
import lustre/attribute as a
import lustre/element/html as h

pub fn view() {
  h.div([s.footer()], [
    h.div([s.footer_links()], {
      use #(title, links) <- list.map(links)
      h.div([s.footer_section()], {
        links
        |> list.map(fn(i) { h.a([s.foot_lk(), a.href(i.0)], [h.text(i.1)]) })
        |> list.prepend(h.div([s.foot_title()], [h.text(title)]))
      })
    }),
    h.div([s.footer_built()], [
      h.text("Gling is proudly built with 💜 in gleam for gleamlins"),
    ]),
  ])
}
