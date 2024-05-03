@external(erlang, "verl", "is_match")
pub fn is_match(
  version version: BitArray,
  requirement requirement: BitArray,
) -> Bool

@external(erlang, "verl", "gte")
pub fn gte(version version_1: BitArray, with version_2: BitArray) -> Bool
