//// BEAM only.

import gleam/list
import gleam/option.{type Option, None, Some}

pub type Definitions {
  Definitions(
    medias_def: List(String),
    selectors_def: List(String),
    class_def: String,
  )
}

pub opaque type Class {
  Class(
    class_name: String,
    class_id: String,
    definitions: Definitions,
    rules: Option(List(Int)),
  )
}

pub fn no_class() {
  let defs = Definitions(medias_def: [], selectors_def: [], class_def: "")
  Class(class_name: "", class_id: "", definitions: defs, rules: None)
}

pub fn class_id(class: Class) {
  class.class_id
}

pub fn class_name(class: Class) {
  class.class_name
}

pub fn rules(class: Class) {
  class.rules
}

pub fn definitions(class: Class) {
  let Definitions(medias, selectors, class) = class.definitions
  [[class], selectors, medias]
  |> list.concat()
}

pub fn set_rules(class: Class, rules: List(Int)) {
  Class(..class, rules: Some(rules))
}

pub fn create(
  class_name class_name: String,
  class_id class_id: String,
  rules rules: Option(List(Int)),
  definitions definitions: Definitions,
) {
  Class(
    class_name: class_name,
    class_id: class_id,
    definitions: definitions,
    rules: rules,
  )
}
