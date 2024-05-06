pub type Model {
  Model(input: String)
}

pub fn init() {
  Model(input: "")
}

pub fn update_input(model: Model, content: String) {
  Model(..model, input: content)
}
