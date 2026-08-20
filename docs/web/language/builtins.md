---
title: Builtins
description: The compiler-resolved operations, what each returns, and which three of them stop the program.
section: language
order: 14
status: stable
---

Builtins are resolved by the compiler rather than linked from a library. Most are written with an
`@` prefix and take type arguments where it makes sense; a few are called like ordinary functions.

```ignis
function main(): i32 {
    let size: u64 = @sizeOf<i32>();
    let align: u64 = @alignOf<i32>();

    return size as i32 + align as i32;
}
```

## Types and layout

| Builtin | Returns | What it does |
| --- | --- | --- |
| `@sizeOf<T>()` | `u64` | Size in bytes, as the backend lays the type out. Emits C `sizeof` |
| `@alignOf<T>()` | `u64` | Required alignment. Emits C `_Alignof` |
| `@typeName<T>()` | `str` | The type's name, resolved to a literal at compile time |
| `typeOf(expr)` | type | The type of an expression, for use where a type is expected |
| `maxOf<T>()`, `minOf<T>()` | `T` | The bounds of a numeric type |

## Reinterpreting values

| Builtin | Returns | What it does |
| --- | --- | --- |
| `@bitCast<T>(value)` | `T` | Reinterprets the bits, without conversion |
| `@pointerCast<T>(ptr)` | `T` | Changes a pointer's pointee type |
| `@integerFromPointer(ptr)` | integer | The address as a number |
| `@pointerFromInteger<T>(value)` | `T` | A number back as a pointer |

These are the sharp ones. They do not check anything for you — that is the entire point of them —
so they belong in FFI glue and in the low-level parts of a library, not in ordinary code.

## Memory and lifetimes

| Builtin | What it does |
| --- | --- |
| `@read<T>(ptr)` | Reads a `T` through a raw pointer |
| `@write<T>(ptr, value)` | Writes a `T` through a raw pointer |
| `@dropInPlace<T>(ptr)` | Runs the drop code for the value at that address |
| `@dropGlue<T>()` | The drop function for a type, as a value |
| `@sliceFromParts(...)` | Builds a slice from a pointer and a length |
| `@hash<T>(value)`, `@eq<T>(left, right)` | The canonical hash and equality for a type |

## Stopping the program

Three builtins end execution, and the difference between them matters.

| Builtin | Emits code | Predictable | Undefined if reached |
| --- | --- | --- | --- |
| `@panic("message")` | Yes — prints and exits | Yes | No |
| `@trap()` | Yes — a trap instruction | Yes | No |
| `@unreachable()` | No | No | **Yes** |

`@panic` is for a logic error you want to hear about. `@trap` is for a low-level assertion where a
message is not worth the code. `@unreachable()` emits nothing at all: it tells the optimizer this
path cannot happen, and if it does happen anyway the behaviour is undefined. Reach for it only when
you can prove the case is impossible — a guess here is worse than no annotation.

All three have type `never`, so a function that always panics still satisfies its declared return
type.

```ignis
function fail(): i32 {
    @panic("fatal");
}
```

## Compile-time

`@compileError(message)` fails the build where it appears.

```ignis
function main(): void {
    @compileError("this should not compile");
}
```

It fires during type checking, which only sees items that survived parsing. An item stripped by
`@configFlag` never reaches the checker, so the error stays quiet unless the item is actually part
of the build — which is what makes it useful for rejecting an unsupported configuration.

`@configFlag(...)` is a directive rather than a builtin, even though it is written like one. It
decides whether an item is included at all, with predicates such as `@platform("linux")` and the
usual boolean combinators.
