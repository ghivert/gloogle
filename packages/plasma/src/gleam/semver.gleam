/// Checks that `requirement` is comprised in `version`.
/// Returns `True` if `requirement` satisfies `version`, `False` otherwise.
@external(erlang, "semver_ffi", "is_match")
pub fn is_match(
  version version: BitArray,
  requirement requirement: BitArray,
) -> Result(Bool, Nil)

/// Checks if `version` is greater or equal than `with`.
@external(erlang, "verl", "gte")
pub fn gte(version version_1: BitArray, with version_2: BitArray) -> Bool
