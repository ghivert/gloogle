import gleam/string
import lustre/attribute
import lustre/element

pub type Dataset {
  Dataset(labels: List(String), data: List(Int))
}

pub fn line_chart(datasets: Dataset) {
  let datasets = attribute.property("datasets", datasets)
  element.element(
    "line-chart",
    [attribute.style([#("display", "block")]), datasets],
    [],
  )
}
