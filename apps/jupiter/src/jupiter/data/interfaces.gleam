import gleam/option.{type Option}

pub type Interfaces {
  Interfaces(
    id: Int,
    package_interface: Option(String),
    gleam_toml: Option(String),
  )
}
