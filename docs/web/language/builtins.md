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
| `maxOf<T>()`, `minOf<T>()` | `T` | The bounds of a numeric type |

`@typeOf` is a reserved builtin name, but the current compiler does not support it yet: a
use of `@typeOf` is rejected as an unsupported construct, and there is no form that puts a
type where a type is expected.

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
| `@readVolatile<T>(ptr)` | Reads a `T` through a volatile load the compiler never elides or merges |
| `@writeVolatile<T>(ptr, value)` | Writes a `T` through a volatile store with the same guarantees |
| `@dropInPlace<T>(ptr)` | Runs the drop code for the value at that address |
| `@dropGlue<T>()` | The drop function for a type, as a value |
| `@sliceFromParts<T>(data, len)` | Builds a `T[]` slice from a data pointer and a length |
| `@hash<T>(value, hasher)` | Hashes `value` (passed as `&T`) with the concrete `Hash` implementation for `T`; the call itself returns nothing |
| `@eq<T>(left, right)` | The canonical equality for a type, returning `boolean` |

The volatile pair exists for memory-mapped device registers and memory shared with code
the compiler cannot see. They take the same arguments as `@read` and `@write`, and the
same rules: the pointer must be `*mut T`, and a literal `null` pointer is a compile-time
error. The C compiler never elides a volatile access, merges it with another one, or
reorders it relative to other volatile accesses — but volatile is not synchronization: the
accesses are not atomic, they are not memory barriers, and they order nothing except other
volatile accesses. Use real synchronization primitives for shared state between threads.

## Zero and fill

`@zeroed<T>()` is a value of `T` whose every byte is zero, and `@splat<T[N]>(value)` is a
fixed array of `N` copies of `value`:

```ignis
record Tables {
    static mut STACK: u8[16384] = @zeroed<u8[16384]>();
    static mut MARKS: u8[8] = @splat<u8[8]>(0xAA);
}
```

`@zeroed` requires a type with a zero value (`A0239`): integers, floats, `boolean`, `char`,
raw pointers, and fixed arrays, records and payload-free enums built from those. A string,
a reference, a function value, a slice, a range, or anything implementing `Drop` has no
zero. `@splat` requires a fixed array type argument (`A0241`) and a `Copy` element type
(`A0242`), and a constant `@splat` holds at most 65536 elements (`A0240`).

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
