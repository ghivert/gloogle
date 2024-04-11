import filespy
import gleam/io
import shellout
import gleam/string
import gleam/list
import gleam/otp/actor
import gleam/option.{type Option, None, Some}

/// Phantom type to indicate no directories are added
pub type NoDirectories

/// Phantom type to indicate directories have been added
pub type HasDirectories

/// Phantom type to indicate builder has no initializer
pub type NoInitializer

/// Phantom type to indicate builder has initializer
pub type HasInitializer

/// Opaque builder type for reloader. Create with `new`.
pub opaque type Builder(state, has_dirs, has_initializer) {
  Builder(
    dirs: List(String),
    callback: Option(fn(state, String) -> state),
    initializer: Option(fn() -> state),
  )
}

/// Construct a new builder
pub fn new() -> Builder(Nil, NoDirectories, NoInitializer) {
  Builder(dirs: [], callback: None, initializer: None)
}

pub fn add_dir(
  builder: Builder(state, has_dirs, has_initializer),
  dir: String,
) -> Builder(state, HasDirectories, has_initializer) {
  Builder(
    dirs: [dir, ..builder.dirs],
    callback: builder.callback,
    initializer: builder.initializer,
  )
}

/// Add a callback to be run after reload
pub fn on_reload(
  builder: Builder(state, has_dirs, has_initializer),
  callback: fn(state, String) -> state,
) -> Builder(state, has_dirs, has_initializer) {
  Builder(
    dirs: builder.dirs,
    callback: Some(callback),
    initializer: builder.initializer,
  )
}

/// Add an initializer
///
/// This will be run in the file watcher process, so it can be useful to, for
/// example, add state to an actor
pub fn set_initializer(
  builder: Builder(state, has_dirs, NoInitializer),
  initializer: fn() -> state,
) -> Builder(state, has_dirs, HasInitializer) {
  Builder(
    dirs: builder.dirs,
    callback: builder.callback,
    initializer: Some(initializer),
  )
}

type Module

type What

@external(erlang, "code", "modified_modules")
fn modified_modules() -> List(Module)

@external(erlang, "code", "purge")
fn purge(module: Module) -> Bool

@external(erlang, "code", "atomic_load")
fn atomic_load(modules: List(Module)) -> Result(Nil, List(#(Module, What)))

/// Start reloader, if no state has been set
pub fn start(builder: Builder(Nil, HasDirectories, NoInitializer)) {
  builder
  |> set_initializer(fn() { Nil })
  |> start_state()
}

/// Start reloader, ensuring that a state has been set
pub fn start_state(builder: Builder(state, HasDirectories, HasInitializer)) {
  filespy.new()
  |> filespy.set_initializer(fn() {
    let assert Some(init) = builder.initializer
    init()
  })
  |> filespy.add_dirs(builder.dirs)
  |> filespy.set_actor_handler(fn(msg, state) {
    let filespy.Change(path, events) = msg

    // todo fold over state... gah!
    list.fold(events, state, fn(state, event) {
      case event {
        filespy.Closed | filespy.Modified -> {
          // Check if path ends in '.gleam'
          case string.ends_with(path, ".gleam") {
            True -> {
              // Rebuild
              let build_result =
                shellout.command(["build"], run: "gleam", in: ".", opt: [])

              case build_result {
                Ok(_output) -> {
                  let mods = modified_modules()
                  list.each(mods, purge)
                  let _ = atomic_load(mods)

                  case builder.callback {
                    Some(cb) -> cb(state, path)
                    None -> state
                  }
                }

                Error(#(_status, message)) -> {
                  message
                  |> io.print_error

                  state
                }
              }
            }
            _ -> state
          }
        }
        _ -> state
      }
    })
    |> actor.continue
  })
  |> filespy.start()
}
// todo:
// - make it so only one callback can be set (maybe one per directory?)
// - default state is nil, default callback is printing reloaded
// - callback returns actor stuff
