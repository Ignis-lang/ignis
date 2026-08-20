---
title: Types and literals
description: The primitive types, what each one holds, and the coercions the compiler will and will not perform.
section: language
order: 1
status: stable
---

Ignis is strongly typed. Annotations are explicit at declaration boundaries, and the compiler
infers within an expression rather than across one.

## Primitives

| Group | Types |
| --- | --- |
| Signed integers | `i8`, `i16`, `i32`, `i64` |
| Unsigned integers | `u8`, `u16`, `u32`, `u64` |
| Floating point | `f32`, `f64` |
| Boolean | `boolean` |
| Text | `char`, `str` |
| Labels | `atom` |
| Unit and bottom | `void`, `never` |

## Text

`str` is a UTF-8 byte string slice. `char` is one Unicode scalar value — not a byte, and not a
grapheme cluster. A character literal that resolves to zero scalars, more than one scalar, or a
surrogate escape is rejected at compile time.

`String` is the owned, heap-backed counterpart. A template literal — `` `hello ${name}` `` —
produces one; see [Template literals](/language/template-literals).

## Atoms

An `atom` is an interned label. Atom literals carry a `:` prefix, and two atoms with the same name
are the same value.

```ignis
function status(ok: boolean): atom {
    if (ok) {
        return :ok;
    }
    return :error;
}
```

## Null

`null` is a literal of type `NullPtr`. It coerces to any pointer type when the surrounding context
supplies one.

```ignis
function probe(): i32 {
    let p: *i32 = null;
    let q: *mut u8 = null;

    if (p == null) {
        return -1;
    }

    return 0;
}
```

Using `null` where a non-pointer type is expected is a compile-time error, and so are dereferencing
it and doing arithmetic on it. It may appear as a match pattern when the scrutinee is
pointer-typed.

## Never

`never` is the bottom type: the type of an expression that never produces a value. `@panic(...)`,
`@trap()` and `@unreachable()` all have it. Because `never` is compatible with every other type, a
function that always panics still satisfies its declared return type.

```ignis
function fail(): i32 {
    @panic("fatal");
}
```

## Beyond the primitives

The table above is the whole set of types the *language* defines. The types you will actually spend
most of your time with — `String`, `Vector<T>`, `HashMap<K, V>`, `Option<T>`, `Result<T, E>` — are
declared in the standard library as ordinary records and enums. Nothing about them is special to
the compiler.

That distinction matters in exactly two places. Library types have to be imported, and they follow
the same ownership rules as anything you write yourself: `String` implements `Drop`, so it moves
rather than copies. Everywhere else you can treat them as types like any other.

| Type | Comes from | Use it for |
| --- | --- | --- |
| `str` | The language | A borrowed, immutable string — every string literal is one |
| `String` | `std::string` | An owned, growable string you can mutate |
| `T[N]` | The language | A fixed-size sequence, sized at compile time |
| `Vector<T>` | `std::vector` | A growable sequence sized at runtime |
| `HashMap<K, V>` | `std::collections` | Lookup by key |
| `HashSet<T>` | `std::collections` | Membership without a payload |
| `BitSet` | `std::collections` | Membership for dense `u32` indexes |
| `Option<T>` | The standard library | A value that may be absent |
| `Result<T, E>` | The standard library | An operation that may fail |

`Option` and `Result` are enums marked `@lang(try)`, which is what lets the `!` operator work on
them — see [enums](/docs/language/enums). Choosing between the rest is covered in
[data structures](/docs/std/data-structures).

## References and pointers

References and raw pointers are separate types, and mutability is part of the type rather than a
property of the binding on the other end.

| Form | Meaning |
| --- | --- |
| `&T` | Shared reference |
| `&mut T` | Exclusive reference |
| `*T` | Raw pointer |
| `*mut T` | Mutable raw pointer |

References are checked by the borrow checker. Raw pointers are not — they exist for FFI and for the
low-level parts of the standard library.
