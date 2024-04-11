import lustre/effect.{type Effect}

pub fn none(model: model) {
  #(model, effect.none())
}

pub fn add_effect(tuple: #(model, Effect(msg)), effect: Effect(msg)) {
  let #(model, fst_effect) = tuple
  #(model, effect.batch([fst_effect, effect]))
}
