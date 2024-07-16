import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/pair
import gleam/string
import sketch/internals/class.{type Class}
import sketch/internals/string as sketch_string

pub type Style {
  ClassName(class_name: #(String, List(Style)))
  Media(query: String, styles: List(Style))
  PseudoSelector(pseudo_selector: String, styles: List(Style))
  Property(key: String, value: String, important: Bool)
}

pub type ComputedProperties {
  ComputedProperties(
    properties: List(String),
    medias: List(MediaProperty),
    classes: List(String),
    pseudo_selectors: List(PseudoProperty),
    indent: Int,
  )
}

pub type MediaProperty {
  MediaProperty(
    query: String,
    properties: List(String),
    pseudo_selectors: List(PseudoProperty),
  )
}

pub type PseudoProperty {
  PseudoProperty(pseudo_selector: String, properties: List(String))
}

fn compute_property(indent: Int, key: String, value: String, important: Bool) {
  let base_indent = sketch_string.indent(indent)
  let important_ = case important {
    True -> " !important"
    False -> ""
  }
  base_indent <> key <> ": " <> value <> important_ <> ";"
}

fn init_computed_properties(indent: Int) {
  ComputedProperties(
    properties: [],
    medias: [],
    classes: [],
    pseudo_selectors: [],
    indent: indent,
  )
}

// Computing of properties

/// The Style data structure being a recursive data, computeProperties traverse
/// the data structure and collect the properties with their context.
pub fn compute_properties(
  cache: Cache,
  properties: List(Style),
  indent: Int,
) -> #(Cache, ComputedProperties) {
  let init = init_computed_properties(indent)
  use #(cache, acc), prop <- list.fold_right(properties, #(cache, init))
  case prop {
    Property(_, _, _) -> #(cache, handle_property(acc, prop))
    Media(_, _) -> handle_media(cache, acc, prop)
    PseudoSelector(_, _) -> handle_pseudo_selector(cache, acc, prop)
    ClassName(styles) -> {
      let #(id, styles) = styles
      let #(cache, class) = compute_class(cache, id, styles)
      #(cache, handle_class_name(acc, class.class_name(class)))
    }
  }
}

fn handle_class_name(props: ComputedProperties, class_name: String) {
  let classes = [class_name, ..props.classes]
  ComputedProperties(..props, classes: classes)
}

fn handle_property(props: ComputedProperties, style: Style) {
  let assert Property(key, value, important) = style
  let css_property = compute_property(props.indent, key, value, important)
  let properties = [css_property, ..props.properties]
  ComputedProperties(..props, properties: properties)
}

fn handle_media(cache: Cache, props: ComputedProperties, style: Style) {
  let assert Media(query, styles) = style
  let #(cache, computed_props) =
    compute_properties(cache, styles, props.indent + 2)
  MediaProperty(
    query: query,
    properties: computed_props.properties,
    pseudo_selectors: computed_props.pseudo_selectors,
  )
  |> list.prepend(props.medias, _)
  |> fn(m) { ComputedProperties(..props, medias: m) }
  |> pair.new(cache, _)
}

fn handle_pseudo_selector(cache: Cache, props: ComputedProperties, style: Style) {
  let assert PseudoSelector(pseudo_selector, styles) = style
  let #(cache, computed_props) =
    compute_properties(cache, styles, props.indent + 2)
  PseudoProperty(
    pseudo_selector: pseudo_selector,
    properties: computed_props.properties,
  )
  |> list.prepend(computed_props.pseudo_selectors, _)
  |> list.append(props.pseudo_selectors)
  |> fn(p) { ComputedProperties(..props, pseudo_selectors: p) }
  |> pair.new(cache, _)
}

// Wrapping of classes.

pub type ComputedClass {
  ComputedClass(
    class_def: String,
    medias_def: List(String),
    selectors_def: List(String),
    name: String,
  )
}

fn wrap_pseudo_selectors(
  id: String,
  indent: Int,
  pseudo_selectors: List(PseudoProperty),
) {
  use p <- list.map(pseudo_selectors)
  sketch_string.wrap_class(id, p.properties, indent, Some(p.pseudo_selector))
}

// Compute classes by using the class definitions, and by wrapping them in the
// correct class declarations, to be CSS compliant.
pub fn compute_classes(
  class_name: String,
  computed_properties: ComputedProperties,
) {
  let ComputedProperties(properties, medias, classes, pseudo_selectors, _) =
    computed_properties
  let class_def = sketch_string.wrap_class(class_name, properties, 0, None)
  let medias_def = {
    use MediaProperty(query, properties, pseudo_selectors) <- list.map(medias)
    let selectors_def = wrap_pseudo_selectors(class_name, 2, pseudo_selectors)
    [query <> " {", sketch_string.wrap_class(class_name, properties, 2, None)]
    |> list.prepend([selectors_def, ["}"]], _)
    |> list.concat()
    |> string.join("\n")
  }
  let selectors_def = wrap_pseudo_selectors(class_name, 0, pseudo_selectors)
  let name = string.trim(string.join(classes, " ") <> " " <> class_name)
  ComputedClass(class_def, medias_def, selectors_def, name)
}

pub fn create_cache() -> Cache {
  init()
}

pub fn compile_class(styles: List(Style)) -> #(String, List(Style)) {
  let st = string.inspect(styles)
  #(st, styles)
  // let content = dict.get(cache.state.cache, st)
  // case content {
  //   Ok(class) -> class
  //   Error(_) -> compute_class(cache.state, st, styles) |> pair.second
  // }
}

pub type Cache {
  Cache(cache: Dict(String, Class), current_id: Int)
}

pub fn init() {
  Cache(cache: dict.new(), current_id: 0)
}

pub fn compute_class(
  cache: Cache,
  class_id: String,
  styles: List(Style),
) -> #(Cache, Class) {
  let class_name = "ccs-" <> int.to_string(cache.current_id)
  let #(cache, properties) = compute_properties(cache, styles, 2)
  compute_classes(class_name, properties)
  |> fn(c: ComputedClass) {
    class.create(
      class_name: c.name,
      class_id: class_id,
      rules: None,
      definitions: class.Definitions(
        medias_def: c.medias_def,
        selectors_def: c.selectors_def,
        class_def: c.class_def,
      ),
    )
  }
  |> fn(class) {
    cache.cache
    |> dict.insert(class_id, class)
    |> fn(c) { Cache(cache: c, current_id: cache.current_id + 1) }
    |> pair.new(class)
  }
}
