import gleam/dynamic.{type Dynamic}

pub type Step {
  Step(index: String, model: Dynamic, msg: Dynamic)
}
