/// Returns the home directory, `$HOME` on Linux.
@external(erlang, "fs_ffi", "home")
pub fn home() -> Result(String, Nil)

@external(erlang, "file", "write_file")
pub fn write_file(destination: String, content: String) -> Result(Nil, Nil)
