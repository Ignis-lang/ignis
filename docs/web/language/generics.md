---
title: Generics
description: Type parameters on functions, records, enums and aliases, and how the compiler specializes them.
section: language
order: 4
status: stable
---

Functions, records, enums and type aliases all take type parameters.

```ignis
function identity<T>(value: T): T {
    return value;
}

record Box<T> {
    public value: T;
}

type Handler<T> = (T) -> void;
```

## Inference at the call site

A type argument is inferred from the arguments when it can be, and written explicitly when you want
a different one.

```ignis
function identity<T>(value: T): T {
    return value;
}

function main(): i32 {
    let x = identity(42);
    let y = identity<i64>(42);

    return x;
}
```

## Instantiating a generic type

```ignis
record Box<T> {
    public value: T;
}

function main(): i32 {
    let b: Box<i32> = Box { value: 42 };

    return b.value;
}
```

## Monomorphization

Generics are not erased and they are not boxed. Before any C is emitted, the compiler specializes
every instantiation into a concrete copy: `Box<i32>` and `Box<f64>` become two distinct C structs,
and `identity<i32>` and `identity<i64>` become two functions.

Two consequences worth knowing:

- There is no runtime cost to a type parameter, and no dynamic dispatch hiding behind one.
- A generic that is never instantiated emits no code at all, so a type error inside it still has to
  be caught by the checker rather than by codegen.

After that pass no type parameter survives anywhere in the program. If you are reading compiler
output and find one, that is a bug in the compiler, not a feature of the language.

## Type aliases

An alias can be generic too, and it is transparent: it names an existing type rather than creating
a new one.

```ignis
type Id = i32;
type Handler<T> = (T) -> void;
```
