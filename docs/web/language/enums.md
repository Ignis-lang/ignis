---
title: Enums and variants
description: Sum types with payloads, methods and static members, and the enums that power the try operator.
section: language
order: 3
status: stable
---

An enum is a closed set of variants. A variant may carry a payload.

```ignis
enum Option<T> {
    SOME(T),
    NONE,
}
```

Variants are reached through `::`, and matching on an enum has to cover every one of them.

## Methods and static members

Enums carry the same member kinds records do: methods, static methods and static fields.

```ignis
enum Priority {
    LOW,
    HIGH,

    DEFAULT_LEVEL: i32 = 1;

    static fromInt(n: i32): Priority {
        if (n > 0) {
            return Priority::HIGH;
        }

        return Priority::LOW;
    }
}
```

## Try-capable enums

An enum marked `@lang(try)` works with the try operator `!`, which unwraps the success variant or
returns the failure variant from the enclosing function.

```ignis
@lang(try)
enum Result<T, E> {
    OK(T),
    ERROR(E),
}

@lang(try)
enum Option<T> {
    SOME(T),
    NONE,
}
```

The attribute demands exactly two variants. The first is the success case, the second the failure
case — the order in the declaration is what decides it, not the names.

```ignis
@lang(try)
enum Result<T, E> {
    OK(T),
    ERROR(E),
}

function divide(a: i32, b: i32): Result<i32, str> {
    if (b == 0) {
        return Result::ERROR("division by zero");
    }

    return Result::OK(a / b);
}

function compute(): Result<i32, str> {
    let x = divide(10, 2)!;
    let y = divide(20, 4)!;

    return Result::OK(x + y);
}
```

See [expressions](/docs/language/expressions) for what `!` desugars to and the rules it enforces.
