//// Random IR for the property tests — see `laws_test`.
////
//// The generators keep the invariants the matcher assumes and nothing enforces
//// at the type level: a `Union` has no duplicate member and no union among its
//// members, a `Record` no duplicate field name. Names come from a small vocabulary so that random
//// types share names and shapes often enough for the laws to meet interesting
//// cases (two `Int`s, a repeated variable), and carry no `package`/`concept`,
//// which the canonical notation does not print.

import data/ir
import gleam/list
import gleam/option.{None}
import qcheck.{type Generator}

/// What a generated type may contain.
pub type Shape {
  Shape(holes: Bool, labels: Bool, methods: Bool)
}

/// Anything the IR can express — queries included (holes, labels).
pub const any = Shape(holes: True, labels: True, methods: True)

/// A stored declaration's type: no hole (the invariant `unify` relies on), but
/// labels and methods.
pub const stored = Shape(holes: False, labels: True, methods: True)

/// What the exact tier sees as one class: no hole, no label.
pub const closed = Shape(holes: False, labels: False, methods: True)

/// A type of at most three levels of nesting.
pub fn type_(shape: Shape) -> Generator(ir.Type) {
  qcheck.sized_from(sized_type(shape, _), qcheck.bounded_int(0, 3))
}

/// A declaration of each of the four kinds, stored-shaped.
pub fn declaration() -> Generator(ir.Declaration) {
  qcheck.from_generators(qcheck.map(type_(stored), ir.Function), [
    qcheck.map(type_(stored), ir.Constant),
    qcheck.map2(qcheck.bounded_int(0, 2), type_(stored), ir.TypeAlias),
    qcheck.map2(
      qcheck.bounded_int(0, 2),
      qcheck.generic_list(constructor(), qcheck.bounded_int(0, 3)),
      ir.TypeDefinition,
    ),
  ])
}

fn constructor() -> Generator(ir.Constructor) {
  qcheck.map2(
    upper_name(),
    qcheck.generic_list(param(stored, 1), qcheck.bounded_int(0, 3)),
    ir.Constructor,
  )
}

fn sized_type(shape: Shape, size: Int) -> Generator(ir.Type) {
  case size {
    0 -> leaf(shape)
    _ ->
      qcheck.from_weighted_generators(#(2, leaf(shape)), [
        #(3, named(shape, size - 1)),
        #(3, function(shape, size - 1)),
        #(1, tuple(shape, size - 1)),
        #(1, record(shape, size - 1)),
        #(1, union(shape, size - 1)),
      ])
  }
}

fn leaf(shape: Shape) -> Generator(ir.Type) {
  let bare =
    qcheck.map(upper_name(), fn(name) { ir.Named(bare_name(name), []) })
  let var = qcheck.map(qcheck.bounded_int(0, 3), fn(i) { ir.Var(i, letter(i)) })
  case shape.holes {
    True ->
      qcheck.from_weighted_generators(#(3, bare), [
        #(2, var),
        #(1, qcheck.constant(ir.Hole)),
      ])
    False -> qcheck.from_weighted_generators(#(3, bare), [#(2, var)])
  }
}

fn named(shape: Shape, size: Int) -> Generator(ir.Type) {
  qcheck.map3(
    upper_name(),
    module(),
    qcheck.generic_list(sized_type(shape, size), qcheck.bounded_int(1, 2)),
    fn(name, module, args) {
      ir.Named(ir.Name(package: "", module:, name:, concept: None), args)
    },
  )
}

fn function(shape: Shape, size: Int) -> Generator(ir.Type) {
  let is_method = case shape.methods {
    True -> qcheck.bool()
    False -> qcheck.constant(False)
  }
  qcheck.map3(
    qcheck.generic_list(param(shape, size), qcheck.bounded_int(0, 3)),
    sized_type(shape, size),
    is_method,
    fn(params, return, is_method) {
      // A method needs a receiver: its first parameter.
      let is_method = is_method && !list.is_empty(params)
      ir.Fn(params:, return:, is_method:)
    },
  )
}

fn param(shape: Shape, size: Int) -> Generator(ir.Param) {
  let label = case shape.labels {
    True -> qcheck.option_from(lower_name())
    False -> qcheck.constant(None)
  }
  qcheck.map3(
    label,
    qcheck.bool(),
    sized_type(shape, size),
    fn(label, spread, type_) { ir.Param(label:, is_spread: spread, type_:) },
  )
}

fn tuple(shape: Shape, size: Int) -> Generator(ir.Type) {
  qcheck.generic_list(sized_type(shape, size), qcheck.bounded_int(0, 3))
  |> qcheck.map(ir.Tuple)
}

/// Field names unique: duplicates are dropped, keeping the first.
fn record(shape: Shape, size: Int) -> Generator(ir.Type) {
  qcheck.generic_list(
    qcheck.map2(lower_name(), sized_type(shape, size), ir.Field),
    qcheck.bounded_int(0, 3),
  )
  |> qcheck.map(fn(fields) {
    fields
    |> list.fold([], fn(acc, field) {
      case list.any(acc, fn(f: ir.Field) { f.name == field.name }) {
        True -> acc
        False -> [field, ..acc]
      }
    })
    |> list.reverse
    |> ir.Record
  })
}

/// Members unique and never unions themselves (`A | (B | C)` is `A | B | C`
/// and prints flat); fewer than two members left is not a union but the member.
fn union(shape: Shape, size: Int) -> Generator(ir.Type) {
  qcheck.generic_list(sized_type(shape, size), qcheck.bounded_int(2, 3))
  |> qcheck.map(fn(members) {
    let flat =
      list.flat_map(members, fn(member) {
        case member {
          ir.Union(inner) -> inner
          other -> [other]
        }
      })
    case list.unique(flat) {
      [single] -> single
      members -> ir.Union(members)
    }
  })
}

fn upper_name() -> Generator(String) {
  qcheck.from_generators(qcheck.constant("Int"), [
    qcheck.constant("String"),
    qcheck.constant("List"),
    qcheck.constant("Result"),
    qcheck.constant("Option"),
    qcheck.constant("Bool"),
  ])
}

fn lower_name() -> Generator(String) {
  qcheck.from_generators(qcheck.constant("a"), [
    qcheck.constant("b"),
    qcheck.constant("with"),
    qcheck.constant("over"),
  ])
}

fn module() -> Generator(String) {
  qcheck.from_generators(qcheck.constant(""), [
    qcheck.constant("gleam/option"),
    qcheck.constant("gleam"),
  ])
}

fn bare_name(name: String) -> ir.Name {
  ir.Name(package: "", module: "", name:, concept: None)
}

fn letter(i: Int) -> String {
  case i {
    0 -> "a"
    1 -> "b"
    2 -> "c"
    _ -> "d"
  }
}

// ─── Structural helpers for the laws ─────────────────────────────────────────

/// The number of nodes in a type, pre-order — what `replace` indexes.
pub fn size(type_: ir.Type) -> Int {
  1 + list.fold(children(type_), 0, fn(acc, child) { acc + size(child) })
}

/// Replace the `index`-th node (pre-order) with `with`.
pub fn replace(type_: ir.Type, index: Int, with: ir.Type) -> ir.Type {
  let #(replaced, _) = do_replace(type_, index, with)
  replaced
}

/// The `index`-th node (pre-order), if any.
pub fn node_at(type_: ir.Type, index: Int) -> Result(ir.Type, Nil) {
  case index {
    0 -> Ok(type_)
    _ -> node_in(children(type_), index - 1)
  }
}

fn node_in(types: List(ir.Type), index: Int) -> Result(ir.Type, Nil) {
  case types {
    [] -> Error(Nil)
    [first, ..rest] -> {
      let n = size(first)
      case index < n {
        True -> node_at(first, index)
        False -> node_in(rest, index - n)
      }
    }
  }
}

/// Drop the source name of every variable, so that a parsed type (which has
/// none) compares equal to the type it was printed from.
pub fn nameless(type_: ir.Type) -> ir.Type {
  map_type(type_, fn(node) {
    case node {
      ir.Var(index, _) -> ir.Var(index, "")
      other -> other
    }
  })
}

fn children(type_: ir.Type) -> List(ir.Type) {
  case type_ {
    ir.Var(..) | ir.Hole -> []
    ir.Named(_, args) -> args
    ir.Tuple(elements) -> elements
    ir.Union(members) -> members
    ir.Record(fields) -> list.map(fields, fn(field) { field.type_ })
    ir.Fn(params, return, _) ->
      list.append(list.map(params, fn(param) { param.type_ }), [return])
  }
}

fn do_replace(type_: ir.Type, index: Int, with: ir.Type) -> #(ir.Type, Int) {
  case index {
    0 -> #(with, -1)
    _ -> {
      let index = index - 1
      case type_ {
        ir.Var(..) | ir.Hole -> #(type_, index)
        ir.Named(name, args) -> {
          let #(args, index) = replace_in(args, index, with)
          #(ir.Named(name, args), index)
        }
        ir.Tuple(elements) -> {
          let #(elements, index) = replace_in(elements, index, with)
          #(ir.Tuple(elements), index)
        }
        ir.Union(members) -> {
          let #(members, index) = replace_in(members, index, with)
          #(ir.Union(members), index)
        }
        ir.Record(fields) -> {
          let #(types, index) =
            replace_in(list.map(fields, fn(f) { f.type_ }), index, with)
          let fields =
            list.map2(fields, types, fn(field, type_) {
              ir.Field(..field, type_:)
            })
          #(ir.Record(fields), index)
        }
        ir.Fn(params, return, is_method) -> {
          let #(types, index) =
            replace_in(list.map(params, fn(p) { p.type_ }), index, with)
          let params =
            list.map2(params, types, fn(param, type_) {
              ir.Param(..param, type_:)
            })
          let #(return, index) = case index < 0 {
            True -> #(return, index)
            False -> do_replace(return, index, with)
          }
          #(ir.Fn(params, return, is_method), index)
        }
      }
    }
  }
}

/// Replace in a list of siblings; a negative index means "already done".
fn replace_in(
  types: List(ir.Type),
  index: Int,
  with: ir.Type,
) -> #(List(ir.Type), Int) {
  let #(index, replaced) =
    list.map_fold(types, index, fn(index, type_) {
      case index < 0 {
        True -> #(index, type_)
        False -> {
          let #(type_, index) = do_replace(type_, index, with)
          #(index, type_)
        }
      }
    })
  #(replaced, index)
}

/// Rebuild a type bottom-up, applying `f` to every node.
pub fn map_type(type_: ir.Type, f: fn(ir.Type) -> ir.Type) -> ir.Type {
  let mapped = case type_ {
    ir.Var(..) | ir.Hole -> type_
    ir.Named(name, args) -> ir.Named(name, list.map(args, map_type(_, f)))
    ir.Tuple(elements) -> ir.Tuple(list.map(elements, map_type(_, f)))
    ir.Union(members) -> ir.Union(list.map(members, map_type(_, f)))
    ir.Record(fields) ->
      ir.Record(
        list.map(fields, fn(field) {
          ir.Field(..field, type_: map_type(field.type_, f))
        }),
      )
    ir.Fn(params, return, is_method) ->
      ir.Fn(
        list.map(params, fn(param) {
          ir.Param(..param, type_: map_type(param.type_, f))
        }),
        map_type(return, f),
        is_method,
      )
  }
  f(mapped)
}

/// Does the type contain a hole?
pub fn has_hole(type_: ir.Type) -> Bool {
  case type_ {
    ir.Hole -> True
    _ -> list.any(children(type_), has_hole)
  }
}

/// The index of a random node of `type_`.
pub fn position(type_: ir.Type) -> Generator(Int) {
  qcheck.bounded_int(0, size(type_) - 1)
}

/// `map` over the parameters of a function.
pub fn map_params(
  params: List(ir.Param),
  f: fn(ir.Param) -> ir.Param,
) -> List(ir.Param) {
  list.map(params, f)
}
