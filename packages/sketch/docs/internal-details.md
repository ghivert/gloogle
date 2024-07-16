# Internal details

Sketch tries to be to CSS what the VDOM is to the DOM: the ultimate pain-free
tool to manage the state of your CSS styles, without having to worry with CSS
while leveraging CSS skills.

## I don't know anything about Sketch!

This documentation focuses on internal and how is working Sketch under-the-hood.
No worry though, just heads up to the [README](https://hexdocs.pm/sketch) to get
an overview of Sketch, and to get it work with your favorite framework!

## Lifecycle

To do this, Sketch tries to maintain a cache of styles, that can be updated
between every render of the DOM, and will update the correct StyleSheet in DOM.
It can be viewed as a virtual stylesheet, in the same way lustre uses a virtual
DOM under the hood to maintain the DOM. In sketch, it's called a "cache".

Sketch has a lifecycle to make it work. After having created a Cache, you have
to call `prepare` before every repaint, and `render` after every repaint.

```txt
                            +--------------+
                            | Create Cache |
                            +--------------+
                                   |
                                   |
                                   |
                                   v
                    +-------------------------------+
                    | Before paint, setup the cache |  <-------+
                    +-------------------------------+          |
                                   |                           |
                                   |                           |
                                   |                           |
                                   v                           |
              +-------------------------------------------+    |
              |                                           |    |
              |      framework paints to the DOM          |    |
              |   and calls class and dynamic functions   |    |
              |            provided by sketch             |    |
              |                                           |    |
              +-------------------------------------------+    |
                                   |                           |
                                   |                           |
                                   |                           |
                                   v                           |
                    +-------------------------------+          |
                    | After paint, render the cache |  --------+
                    +-------------------------------+
```

- `prepare` setup the Cache in order to diff the old styles with the new styles.
  If `prepare` is not called before every repaint, the stylesheet will not diff
  styles, and it will continue to append styles to the stylesheet.

- `render` accepts the cache and will inject the stylesheet in the DOM or the
  document in the browser, and will returns the stylesheet as a string on BEAM.

## Different architectures

Sketch differs a little bit according to the runtime it's running on. Sketch has
two different runtimes: the first with an (almost) pure gleam implementation,
the second with a native JS implementation, to avoid bad async behaviors and to
maintain high performance.

### Global overview

```txt
                                  +--------------+
                                  | Create Cache |
                                  +--------------+
                                         |
                                         |
                                         |
                                         v
                          +-------------------------------+
                          | Before paint, setup the cache |  <----------------------+
                          +-------------------------------+                         |
                               |                     |                              |
                               |                     |                              |
                               |                     |                              |
                               v                     v                              |
+ JS ---------------------------------+   + BEAM ------------------------------+    |
|                                     |   |                                    |    |
|     Stores the cache globally       |   |      Stores the cache by actor     |    |
|      just before the render         |   |           in an ETS table          |    |
|                                     |   |                                    |    |
+-------------------------------------+   +------------------------------------+    |
                   |                                         |                      |
                   |                                         |                      |
                   |                                         |                      |
                   v                                         v                      |
+ JS ---------------------------------+   + BEAM ------------------------------+    |
|                                     |   |                                    |    |
|   Gleam generates HTML and calls    |   |   Gleam generates HTML and calls   |    |
|    class and dynamic functions      |   |    class and dynamic functions     |    |
|      provided by sketch using       |   |      provided by sketch using      |    |
|     native implementation in JS     |   |   an OTP actor and gleam implem    |    |
|                                     |   |                                    |    |
+-------------------------------------+   +------------------------------------+    |
                   |                                          |                     |
                   |                                          |                     |
                   |                                          |                     |
                   v                                          v                     |
     + JS -----------------------+              +---------------------------+       |
     |  After paint, render the  |              |  After paint, render the  |       |
     |    cache on browser       |              |    cache with browser     |       |
     +---------------------------+              +---------------------------+       |
               |                                                |                   |
               +------------------------------------------------+-------------------+
```

In gleam implementation, every cache is modeled as an actor, while in JS, every
cache is modeled as a mutable object. The algorithm is identical in both case. A
virtual stylesheet is made of a cache, and finally an abstract stylesheet,
following the same interface as a CSSStyleSheet on browser.

Every cache has a concept of an active and a passive class list. The active
class list is the list maintained during the current render. The passive class
list is the list from the previous render. The goal here is, every time we have
to compute a class, to check if that class has already been computed in the last
render. If it is, then the class is transferred from the passive class list to
the active class list. At the end of render, a diff is made between the two
lists, the new classes, created during the render and pushed to the active class
list are added to the stylesheet, the old styles only present in the passive
class list are removed from the stylesheet, and finally, the stylesheet is
constructed. The active class list becomes the passive class list, and the loop
starts again.

## Some notes on side-effects

Unfortunately, and because of the nature of the different frameworks and of CSS,
Sketch is doing some side-effects in background, to collect the styles and to
push them in the browser. Maybe it could be removed in the future, but it would
involve work with different maintainers of different packages, and it would take
a lot of time and energy. It's not on plan right now, but rather to focus on
correct UX and to find good ways of doing things. When the dust will settle and
that API will be stable, then we could take some time to figure out how to get
rid of side-effects. In the meantime, if you plan to integrate Sketch in your
framework, and need some access to underlying and internals, open an issue with
your use case, I'd more than happy to help on that point and reduce
side-effects.
