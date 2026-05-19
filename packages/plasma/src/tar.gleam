import gleam/dynamic.{type Dynamic}

/// Extract the content of the tar file stored as binary format
/// to the destination. Decompress the file if compressed.
@external(erlang, "tar_ffi", "extract_binary")
pub fn extract_binary(
  binary: BitArray,
  destination destination: String,
) -> Result(Dynamic, Nil)

/// Extract the content of the tar file stored at path `file_path`
/// to the destination. Decompress the file if compressed.
@external(erlang, "tar_ffi", "extract")
pub fn extract(
  file_path: String,
  destination destination: String,
) -> Result(Dynamic, Nil)

/// Delete the file at path `file_path`.
@external(erlang, "tar_ffi", "remove")
pub fn remove(file_path: String) -> String
