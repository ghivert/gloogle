# radiate

[![Package Version](https://img.shields.io/hexpm/v/radiate)](https://hex.pm/packages/radiate)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/radiate/)

Hot reloading while in development for Gleam!

## Introduction

Radiate will watch any directory you specify for changes. When a file is
changed in that directory, it'll check if it's a Gleam file, and if it is, the
project will be recompiled and all modified modules will be reloaded, without
having to restart the BEAM VM.

## Quick start

```gleam
// On main.gleam
import gleam/erlang/process
import gleam/io
import radiate
import message

pub fn main() {
  let _ =
    radiate.new()
    |> radiate.add_dir("src")
    |> radiate.start()

  let timer_subject = process.new_subject()

  print_every_second(timer_subject)
}

fn print_every_second(subject: process.Subject(Nil)) {
  process.send_after(subject, 1000, Nil)

  let _ = process.receive(subject, 1500)
  io.println(message.get_message())

  print_every_second(subject)
}

// On message.gleam
pub fn get_message() -> String {
  "Hello!"
}
```

When you first run this, it'll print "Hello!" every second.

Now, go ahead and change the text `get_message` returns to `"Hello, world!"`, and save the file.

As soon as you save, the message printed is changed to "Hello, world!", without needing to restart the program!

## Adding a callback

You can add callbacks to be run every time code is reloaded through `on_reload`:

```gleam
import radiate
import gleam/io

pub fn main() {
  let _ = radiate.new()
    |> radiate.add_dir("src")
    |> radiate.on_reload(fn (_state, path) {
      io.println("Change in " <> path <> ", reloading!")
    })
    |> radiate.start()
}
```


## Installation

This package can be added to your Gleam project:

```sh
gleam add radiate
```

and its documentation can be found at <https://hexdocs.pm/radiate>.
