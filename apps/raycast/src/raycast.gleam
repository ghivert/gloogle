import gleam/option
import redraw

pub type DetailProps {
  DetailProps(
    markdown: String,
    is_loading: Bool,
    actions: option.Option(redraw.Component),
    metadata: option.Option(redraw.Component),
    navigation_title: option.Option(String),
  )
}

@external(javascript, "@raycast/api", "Detail")
fn ffi_detail(props: a) -> redraw.Component

fn detail() -> fn(DetailProps) -> redraw.Component {
  redraw.to_component_("Detail", ffi_detail)
}

pub fn main() {
  let detail = detail()
  detail(DetailProps(
    markdown: "Hello from Gleam!",
    is_loading: True,
    actions: option.None,
    metadata: option.None,
    navigation_title: option.None,
  ))
}
