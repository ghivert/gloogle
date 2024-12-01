import lustre/attribute
import lustre/element

pub type Dataset {
  Dataset(labels: List(String), data: List(Int))
}

pub fn line_chart(datasets: Dataset) {
  let datasets = attribute.property("datasets", datasets)
  let attributes = [attribute.style([#("display", "block")]), datasets]
  element.element("line-chart", attributes, [])
}

pub fn bar_chart(color: String, datasets: Dataset) {
  let datasets = attribute.property("datasets", datasets)
  let color = attribute.property("color", color)
  let attributes = [attribute.style([#("display", "block")]), datasets, color]
  element.element("bar-chart", attributes, [])
}
