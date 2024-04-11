import gleam/result
import lustre/effect
import plinth/javascript/storage
import sketch

const settings_key = "lustre-debugger-color"

pub type Colors {
  Colors(
    background: String,
    shadow: String,
    primary: String,
    editor_fg: String,
    editor_bg: String,
    gutter: String,
    syntax_comment: String,
    button: String,
    function: String,
    nil: String,
    bool: String,
    constant: String,
    bit_array: String,
    utf_codepoint: String,
    string: String,
    number: String,
    custom_type: String,
    regex: String,
    date: String,
  )
}

pub type ColorScheme {
  AyuDark
  AyuLight
  Gleam
}

pub fn cs_to_string(color_scheme) {
  case color_scheme {
    AyuDark -> "Ayu Dark"
    AyuLight -> "Ayu Light"
    Gleam -> "Gleam"
  }
}

pub fn cs_from_string(key) {
  case key {
    "Ayu Dark" -> AyuDark
    "Ayu Light" -> AyuLight
    "Gleam" -> Gleam
    _ -> Gleam
  }
}

pub fn themes() {
  [AyuDark, AyuLight, Gleam]
}

pub const ayu_dark = Colors(
  background: "#111",
  shadow: "#333",
  primary: "#ffcc66",
  editor_fg: "#cccac2",
  editor_bg: "#242936",
  gutter: "#8a919966",
  syntax_comment: "#b8cfe680",
  button: "#ffd173",
  function: "#ffd173",
  nil: "#ffad66",
  bool: "#dfbfff",
  constant: "#ffad66",
  bit_array: "#d5ff80",
  utf_codepoint: "#f28779",
  string: "#d5ff80",
  number: "#5ccfe6",
  custom_type: "#73d0ff",
  regex: "#95e6cb",
  date: "#dfbfff",
)

pub const ayu_light = Colors(
  background: "white",
  shadow: "#ccc",
  primary: "#ffd596",
  editor_fg: "#5c6166",
  editor_bg: "#f8f9fa",
  gutter: "#8a919966",
  syntax_comment: "#787b8099",
  button: "#F2AE49",
  function: "#F2AE49",
  nil: "#fa8d3e",
  bool: "#a37acc",
  constant: "#fa8d3e",
  bit_array: "#86b300",
  utf_codepoint: "#f07171",
  string: "#86b300",
  number: "#55b4d4",
  custom_type: "#399ee6",
  regex: "#4cbf43",
  date: "#a37acc",
)

pub const gleam = Colors(
  background: "#2f2f2f",
  shadow: "#2f2f2f",
  primary: "#ffaff3",
  editor_fg: "#fefefc",
  editor_bg: "#292d3e",
  gutter: "#8a919966",
  syntax_comment: "#848484",
  button: "#ffaff3",
  function: "#ffd596",
  nil: "#d4d4d4",
  bool: "#ff6262",
  constant: "#d4d4d4",
  bit_array: "#c8ffa7",
  utf_codepoint: "#c8ffa7",
  string: "#c8ffa7",
  number: "#a6f0fc",
  custom_type: "#9ce7ff",
  regex: "#fdffab",
  date: "#ffddfa",
)

pub fn ayu_dark_class() {
  sketch.class([
    sketch.property("--background", ayu_dark.background),
    sketch.property("--shadow", ayu_dark.shadow),
    sketch.property("--primary", ayu_dark.primary),
    sketch.property("--editor-fg", ayu_dark.editor_fg),
    sketch.property("--editor-bg", ayu_dark.editor_bg),
    sketch.property("--gutter", ayu_dark.gutter),
    sketch.property("--syntax-comment", ayu_dark.syntax_comment),
    sketch.property("--button", ayu_dark.button),
    sketch.property("--function", ayu_dark.function),
    sketch.property("--nil", ayu_dark.nil),
    sketch.property("--bool", ayu_dark.bool),
    sketch.property("--constant", ayu_dark.constant),
    sketch.property("--bit-array", ayu_dark.bit_array),
    sketch.property("--utfcodepoint", ayu_dark.utf_codepoint),
    sketch.property("--string", ayu_dark.string),
    sketch.property("--number", ayu_dark.number),
    sketch.property("--custom-type", ayu_dark.custom_type),
    sketch.property("--regex", ayu_dark.regex),
    sketch.property("--date", ayu_dark.date),
  ])
  |> sketch.to_lustre()
}

pub fn ayu_light_class() {
  sketch.class([
    sketch.property("--background", ayu_light.background),
    sketch.property("--shadow", ayu_light.shadow),
    sketch.property("--primary", ayu_light.primary),
    sketch.property("--editor-fg", ayu_light.editor_fg),
    sketch.property("--editor-bg", ayu_light.editor_bg),
    sketch.property("--gutter", ayu_light.gutter),
    sketch.property("--syntax-comment", ayu_light.syntax_comment),
    sketch.property("--button", ayu_light.button),
    sketch.property("--function", ayu_light.function),
    sketch.property("--nil", ayu_light.nil),
    sketch.property("--bool", ayu_light.bool),
    sketch.property("--constant", ayu_light.constant),
    sketch.property("--bit-array", ayu_light.bit_array),
    sketch.property("--utfcodepoint", ayu_light.utf_codepoint),
    sketch.property("--string", ayu_light.string),
    sketch.property("--number", ayu_light.number),
    sketch.property("--custom-type", ayu_light.custom_type),
    sketch.property("--regex", ayu_light.regex),
    sketch.property("--date", ayu_light.date),
  ])
  |> sketch.to_lustre()
}

pub fn gleam_class() {
  sketch.class([
    sketch.property("--background", gleam.background),
    sketch.property("--shadow", gleam.shadow),
    sketch.property("--primary", gleam.primary),
    sketch.property("--editor-fg", gleam.editor_fg),
    sketch.property("--editor-bg", gleam.editor_bg),
    sketch.property("--gutter", gleam.gutter),
    sketch.property("--syntax-comment", gleam.syntax_comment),
    sketch.property("--button", gleam.button),
    sketch.property("--function", gleam.function),
    sketch.property("--nil", gleam.nil),
    sketch.property("--bool", gleam.bool),
    sketch.property("--constant", gleam.constant),
    sketch.property("--bit-array", gleam.bit_array),
    sketch.property("--utfcodepoint", gleam.utf_codepoint),
    sketch.property("--string", gleam.string),
    sketch.property("--number", gleam.number),
    sketch.property("--custom-type", gleam.custom_type),
    sketch.property("--regex", gleam.regex),
    sketch.property("--date", gleam.date),
  ])
  |> sketch.to_lustre()
}

pub fn choose_color_scheme() {
  storage.local()
  |> result.try(storage.get_item(_, settings_key))
  |> result.map(cs_from_string)
  |> result.unwrap(Gleam)
}

pub fn save_color_scheme(color_scheme: ColorScheme) {
  use _ <- effect.from()
  let _value = {
    use local <- result.try(storage.local())
    let cs_s = cs_to_string(color_scheme)
    storage.set_item(local, settings_key, cs_s)
  }
  Nil
}

pub fn get_color_scheme_class(color_scheme: ColorScheme) {
  case color_scheme {
    AyuLight -> ayu_light_class()
    AyuDark -> ayu_dark_class()
    Gleam -> gleam_class()
  }
}
