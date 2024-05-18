import lustre/effect.{type Effect}

pub fn none(model: model) {
  #(model, effect.none())
}

pub fn effect(model: model, effect: Effect(msg)) {
  #(model, effect)
}

pub fn effects(model: model, effects: List(Effect(msg))) {
  #(model, effect.batch(effects))
}

pub fn add_effect(tuple: #(model, Effect(msg)), effect: Effect(msg)) {
  let #(model, fst_effect) = tuple
  #(model, effect.batch([fst_effect, effect]))
}
