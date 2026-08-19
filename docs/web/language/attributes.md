---
title: Attributes and directives
description: The @ annotations on declarations and parameters, and the compile-time directive surface.
section: language
order: 12
status: experimental
---

Attributes are written `@name` or `@name(args)` and attach to a declaration.

## Declaration attributes

| Attribute | Applies to | Effect |
| --- | --- | --- |
| `@implements(...)` | Record, enum | Lang traits (`Drop`, `Clone`, `Copy`) or a user-defined trait |
| `@packed` | Record | Removes padding between fields in the emitted C struct |
| `@aligned(N)` | Record | Sets a minimum alignment |
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

```ignis
extern rt {
    function release(@takes ptr: *mut void): void;
}

function forEach(@noescape f: (i32) -> void, data: *i32, len: i32): void {
    return;
}
```

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
