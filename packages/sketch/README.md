# Sketch

Sketch is a module providing CSS-in-Gleam in its simpler form. Sketch does not
try to add complicated API on top of CSS. If you have CSS knowledge, you'll feel
right at home, with all the niceties offered by Sketch, i.e. type-checking of
sizes and push-to-browser stylesheets of your classes, as well as SSR support.

Sketch has currently only one run mode: directly in your browser to leverage on
all abilities of the JS runtime. It also allows you to build three types of CSS
classes: dynamic ones, changing over time, static ones, compiled once at each
render, and memoized ones, compiled once and for all, and reused during the
entire lifetime of the application, just like classic CSS stylesheets.

Sketch is thought to interact nicely with [Lustre](https://hexdocs.pm/lustre/),
but can also be used directly in your vanilla Gleam application or in your
fully-featured application. This should probably only be considered to create
custom framework or to integrate Sketch in your favorite framework, because
Sketch has its own lifecycle to render styles. More informations can be found in
the docs.

Sketch targets both the JS and the BEAM ecosystem. You can use it transparently
no matter the target you're using. Sketch works in frontend, backend, SSR, SSG,
with OTP actors or on Node. While sketch tries to be transparent, it does not
compromise with performances, and tries to leverage on every abilities of the
different platforms (and uses a custom renderer both for BEAM and JS, to ensures
sparkling performances in your browser)!

The rest of this README will focus to get you started quickly with a lustre
example, and some details on the available functions. If you want to know more
on how sketch is working, how to use lustre with SSR or SSG, heads up to the
documentation directly! Bonus: if you're using server components, just follow
this guide, everything will be working directly!

## Installation

Sketch is published on [Hex](https://hex.pm/packages/sketch). Add it to your
project by using the gleam CLI.

```bash
gleam add sketch
```

## Setup

If you're using Lustre (which is strongly recommended), you can just use the
[`sketch/lustre.setup`](https://hexdocs.pm/sketch/sketch/lustre.html#setup)
function and
[`sketch/lustre.compose`](https://hexdocs.pm/sketch/sketch/lustre.html#compose)
function.

Otherwise, you have to follow the lifecycle of Sketch, and use the three
low-level functions
[`create_cache`](https://hexdocs.pm/sketch/sketch.html#create_cache),
[`prepare`](https://hexdocs.pm/sketch/sketch.html#prepare) and
[`render`](https://hexdocs.pm/sketch/sketch.html#render). Create the cache with
[`create_cache`](https://hexdocs.pm/sketch/sketch.html#create_cache) and before
every repaint of your application, call
[`prepare`](https://hexdocs.pm/sketch/sketch.html#prepare). After the repaint,
synchronously, call [`render`](https://hexdocs.pm/sketch/sketch.html#render),
and let the magic happen. Heads up in the docs for more details.

## Example with Lustre — Frontend

```gleam
import gleam/int
import lustre
import lustre/element/html
import lustre/event
import sketch
import sketch/lustre as sketch_lustre
import sketch/media
import sketch/options as sketch_options
import sketch/size.{px}

pub type Model =
  Int

pub type Msg {
  Increment
  Decrement
}

pub fn main() {
  let init = fn(_) { 0 }

  let assert Ok(cache) =
    sketch_options.document()
    |> sketch_lustre.setup()

  let assert Ok(_) =
    view
    |> sketch_lustre.compose(cache)
    |> lustre.simple(init, update, _)
    |> lustre.start("#app", Nil)
}

fn update(model: Model, msg: Msg) {
  case msg {
    Increment -> model + 1
    Decrement -> model - 1
  }
}

fn main_class() {
  sketch.class([
    sketch.background("red"),
    sketch.display("flex"),
    sketch.flex_direction("row"),
    sketch.gap(px(12)),
    sketch.padding(px(12)),
    sketch.hover([sketch.background("yellow")]),
    sketch.media(media.max_width(px(450)), [
      sketch.background("purple"),
      sketch.hover([sketch.background("white")]),
    ]),
  ])
  |> sketch.to_lustre()
}

fn color_class(model: Model) {
  let back = case model % 3 {
    0 -> "blue"
    _ -> "green"
  }
  let id = "color-" <> back
  sketch.dynamic(id, [sketch.background(back)])
  |> sketch.to_lustre()
}

fn button_class() {
  [sketch.cursor("crosshair"), sketch.font_size(px(14))]
  |> sketch.class()
  |> sketch.to_lustre()
}

fn view(model: Model) {
  html.div([main_class()], [
    html.button([event.on_click(Decrement), button_class()], [html.text(" - ")]),
    html.div([color_class(model)], [html.text(int.to_string(model))]),
    html.button([event.on_click(Increment), button_class()], [html.text(" + ")]),
  ])
}
```

## Compiling static classes

Sketch exposes a single function
[`class`](https://hexdocs.pm/sketch/sketch.html#class) allowing you to build
your class. The first time your function is called, the corresponding styles
will be compiled into CSS rules, and pushed in your browser or your `<style>`
stylesheet. Every time you'll call the function during the render, no
computation will be done, the class name will be returned, thanks to virtual
stylesheet.

```gleam
import sketch

fn my_class() -> String {
  sketch.class([
    sketch.display("flex"),
    sketch.flex_direction("column"),
  ])
  |> sketch.to_class_name()
}
```

## Compiling dynamic classes

Sketch exposes another function
[`dynamic`](https://hexdocs.pm/sketch/sketch.html#dynamic) allowing you to build
a dynamic class, changing over time. The first time the function is called, the
properties in the declaration will be compiled into CSS, and a class with the
name of the ID will be pushed in the browser. When a class with the same ID is
found once again during the render, it will be kept, and reused. `dynamic`
allows you to build different dynamic classes, with different ID. When you're
generating the ID, if it does not exists in the stylesheet, it will be pushed in
it, and kept at least till the next render. While a `class` function generates
_one_ class at every render, a `dynamic` function generates _multiple_ classes
at every render.

An ID _should be provided_ at the moment, because the runtime is unable to
distinguish between the dynamic classes, or at a high cost during computations:
finding the ID of the new class to generate _is_ a hard task by itself. While a
solution is awaited to be found, be careful to provide a unique id for your
dynamic class.

```gleam
import gleam/bool
import sketch

fn my_dynamic_class(is_column: Bool) -> String {
  // id is unfortunately required at the moment.
  let id = "column-" <> bool.to_string(is_column)
  sketch.dynamic(id, [
    sketch.display("flex"),
    case is_column {
      True -> sketch.flex_direction("column")
      False -> sketch.flex_direction("row")
    }
  ])
  |> sketch.to_class_name()
}
```

## Memoizing a class

Sketch exposes a way to memoize a class:
[`memo`](https://hexdocs.pm/sketch/sketch.html#memo). `memo` allows to keep a
generated class _for ever_. When called on a class, sketch will preserves the
class for ever, and will includes the class in every render, as long as the
virtual stylesheet exists. This allows to skip all computations work for every
class computations later, and to increase performances render. _Be careful, a
class marked as `memo` will be then considered immutable and can never change._

## Using media queries and pseudo-selectors

Because we're in CSS-in-Gleam, we can leverage on the full CSS power, contrarily
to inline styling. This mean we can use media queries and pseudo-selectors! You
only need to call the proper functions, and sketch will take care of the rest.

```gleam
import sketch
import sketch/media
import sketch/size.{px}

fn my_class() {
  sketch.class([
    sketch.display("flex"),
    sketch.flex_direction("row"),
    sketch.background("red"),
    sketch.hover([
      sketch.background("blue"),
    ]),
    sketch.media(media.max_width(px(320)), [
      sketch.flex_direction("column"),
      sketch.hover([
        sketch.background("green"),
      ]),
    ]),
  ])
  |> sketch.to_lustre()
}
```

The example above will be compiled to the following CSS.

```css
.css-001 {
  display: flex;
  flex-direction: row;
  background: red;
}

.css-001:hover {
  background: blue;
}

@media (max-width: 320px) {
  .css-001 {
    flex-direction: column;
  }

  .css-001:hover {
    background: green;
  }
}
```

## Usage with Lustre — Details

[Lustre](https://hexdocs.pm/lustre/) is the recommended framework for frontend
development in Gleam. Sketch tries to simplify as much the development with
Lustre. That's why Sketch exposes a
[`setup`](https://hexdocs.pm/sketch/sketch/lustre.html#setup) function. This
function creates a cache, and returns it. Another function,
[`compose`](https://hexdocs.pm/sketch/sketch/lustre.html#compose) allows to
easily compose the cache with the view function of Lustre. It acts as a "hook"
(lustre does not officially supports hooks right now): it setups the cache
before the view, and render the stylesheet after the view has executed. It tries
to be side-effect free in the `view` in order to have a predictable render in
Lustre, and stick with the Elm architecture mindset.

Once setuped, you can use classes in your Lustre views:
[`to_lustre()`](https://hexdocs.pm/sketch/sketch.html#to_lustre). Just use it in
place of
[`to_class_name()`](https://hexdocs.pm/sketch/sketch.html#to_class_name) to get
a Lustre attribute and use it in your views.

```gleam
import gleam/list
import lustre/element/html
import sketch

// With a pipeline.
fn my_view() {
  [sketch.background("red")]
  |> sketch.class()
  |> sketch.to_lustre()
  |> list.repeat(1)
  |> html.div(_, [])
}

// With a dynamic class.
fn my_other_view(model: Bool) {
  let color = case model {
    True -> "red"
    False -> "blue"
  }
  let id = "my-other-view-" <> color
  html.div(
    [sketch.to_lustre(sketch.dynamic(id, [sketch.background(color)]))],
    [],
  )
}
```

### Helpers for creating elements

It's a common feature in CSS-in-JS to have something like

```js
const MyComponent = styled.div`
  display: flex;
  flex-direction: column;
`
```

While we can't target the exact same thing with sketch (because Gleam does not
allow to execute code like this), we have a somewhat similar interface:
[`sketch/lustre/element.element()`](https://hexdocs.pm/sketch/sketch/lustre.html#element).
`sketch/lustre/element` exposes 4 functions to help you build your custom
components easily, by leveraging on Gleam and its type-checker.
`sketch/lustre/element.element()` forwards the attributes and children to
`lustre/element.element()`, and adds styles along the way!

```gleam
import sketch
import sketch/lustre/element as sketch_element

/// my_component will be infered as
///   my_component: fn (Attributes(msg), List(Element(msg))) -> Element(msg)
fn my_component(attributes, children) {
  sketch_element.element("div", attributes, children, [
    sketch.display("flex"),
    sketch.flex_direction("column"),
  ])
}
```

While this could feel at first too much, with lot of boilerplate, you could
define a snippet in your favorite editor to create such elements more easily.
It's only 2 more lines of code than a standard CSS-in-JS framework, but here you
leverage the full power of Gleam and Lustre! Give a try, it could quickly become
your de-facto way to define partially-applied functions!

> You may wonder why there's no namespace like `sketch/lustre/element/html` like
> Lustre does. We're still trying to figure out the best API to use for sketch
> to integrate properly in Lustre! For sure, such an interface will be added
> along the way! Meanwhile, you only have to type the tag name by hand, instead
> of calling the function, and you still can leverage on the different features
> of sketch, like `memo` & `dynamic`!

> If you have some feedbacks on that interface, feel free to open a discussion,
> or talk about it on the gleam Discord! We're constantly trying to improve it,
> to provide a first class Developer Experience!

`sketch/lustre/element` has 4 functions, mainly to be able to simply use `class`
& `dynamic`, and putting some `memo` on them if you need it.

## Use with Shadow DOM

Sketch can work with a Shadow DOM, in order to hide the compiled styles from the
rest of the application. To do it, you can use
[`plinth`](https://github.com/CrowdHailer/plinth). This allows to create a
`ShadowRoot`, to use
[`sketch/options.shadow_root()`](https://hexdocs.pm/sketch/sketch/options.html#shadow_root).
In the same way you can initialize the cache to render in document or in a
`style` node, you can now use a Shadow Root to paint styles in your application!

## Some opinions on properties

A lot of properties are accessible directly through the `sketch` package. But
with time, some could be added, and new features for existing features can
appear. That's why sketch will never try to be on your way: at any time you can
access [`property()`](https://hexdocs.pm/sketch/sketch.html#property), which
allows you to push any arbitrary property in a class. Another thing is ç that
sketch will always let you access raw, low-level properties. If you're trying to
use something like `sketch.width("auto")` and the property does not support
String, look for a variant with an underscore (`_`), it should fullfill your
needs, like `sketch.width_("auto")`! In case something is missing or a property
does not have its underscore alternative,
[open an issue — or better, a PR — on the repo!](https://github.com/ghivert/sketch)
