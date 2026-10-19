---
title: Attributes and directives
description: The @ annotations on declarations and parameters, and the compile-time directive surface.
section: language
order: 13
status: experimental
---

Attributes are written `@name` or `@name(args)` and attach to a declaration.

## Declaration attributes

| Attribute | Applies to | Effect |
| --- | --- | --- |
| `@implements(...)` | Record, enum | Lang traits (`Drop`, `Clone`, `Copy`) or a user-defined trait |
| `@packed` | Record | Removes padding between fields in the emitted C struct |
| `@aligned(N)` | Record | Sets a minimum alignment |
| `@threadLocal` | `static mut` field | Gives every thread its own copy of the field |
| `@lang(try)` | Enum | Marks the enum try-capable, for the `!` operator |
| `@cold` | Function | Marks the function unlikely to run |
| `@inline`, `@inline(always)`, `@inline(never)` | Function | Inlining hint, force, or refusal |
| `@externName("...")` | Function | Overrides the C symbol name |
| `@extension(Type)`, `@extension(Type, mut)` | Function | Turns the function into a method on `Type` |
| `@deprecated`, `@deprecated("...")` | Declaration | Warns at every use |
| `@allow(...)`, `@warn(...)`, `@deny(...)` | Declaration | Changes a lint's level for that item |
| `@test` | Function | Registers a native test case |
| `@directive(...)` | Function | Declares a compile-time directive |

The `inline` modifier is written before `function`:

```ignis
inline(always) function hotPath(x: i32): i32 {
    return x + 1;
}
```

## Parameter attributes

| Attribute | Effect |
| --- | --- |
| `@takes` | An extern parameter that consumes ownership |
| `@noescape` | A closure parameter that will not outlive the call |
| `@sendable` | A function parameter whose argument may run on another thread (only on function types, A0255) |

```ignis
extern rt {
    function release(@takes ptr: *mut void): void;
}

function forEach(@noescape f: (i32) -> void, data: *i32, len: i32): void {
    return;
}
```

## @threadLocal

`@threadLocal` on a `static mut` field gives every thread its own copy of it. Reads and writes
look like any other `static mut` access:

```ignis
record Scheduler {
    @threadLocal
    static mut CURRENT: *mut Scheduler;
}
```

- Only a `static mut` field can be thread-local. On an immutable `static` or an instance field
  the attribute is an error (A0259); on any other declaration it is an unknown attribute (A0117).
- The initializer is a constant expression, or absent and the field starts zeroed, as for any
  `static mut` (A0230, A0239). Each thread starts from that value.
- The type must not run a drop (A0260): nothing runs when a thread exits.
- A thread-local cannot be borrowed (A0261): no `&` or `&mut` of it or of a field or element of
  it, no `&self` or `&mut self` method call on it, and no slice of a fixed array stored in it.
  The reference would dangle once its thread exits. Copy the value out, or call a method that
  takes `self`. Access through a pointer stored in a thread-local, such as
  `(*Scheduler::CURRENT).field`, reaches the pointee and is allowed.

The C backend emits the global as `static _Thread_local`; the QBE backend emits `thread` data.

## @test

`@test` marks a top-level function as a case for `ignis test`. The contract is strict, and every
part of it is checked: top level, not `extern`, not generic, no parameters, returns `void`.

```ignis
import Test from "std::test";

@test
function smoke(): void {
    Test::assert(true);
    return;
}
```

## Compile-time directives

`@directive(...)` declares a function that runs during compilation rather than at runtime. The
metadata is given as named arguments — `target`, `phase`, `effect`, and optionally `group` and
`capabilities`.

```ignis
import Compile from "std::compile";

@directive(target: "record", phase: check, effect: diagnose)
function derive(context: Compile::Context, target: Compile::ItemReference): void {
    Compile::error(context, target, "records must satisfy this invariant");
    return;
}
```

The recognized phases are `check`, `expand`, `collect`, `finalize` and `transform`. `std::compile`
is compile-time only and never links into a runtime binary.

This surface is the least settled part of the language. What works today: directive declarations
and uses are validated and scheduled before lowering, capability checks run in a default-deny
sandbox, and the diagnostic calls `Compile::error`, `Compile::warning` and `Compile::note` execute.
What does not: generated item insertion and its semantic reintegration. The rest of the
`std::compile` surface exists as opaque compile-time handles, and unsupported generation operations
are hard errors rather than silent no-ops.
