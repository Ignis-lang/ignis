---
title: Pattern matching
description: match expressions, if let, while let and let else — one pattern grammar across all four.
section: language
order: 6
status: stable
---

`match` is an expression. Its arms take an expression body or a block, and an arm can carry a guard.

```ignis
@lang(try)
enum Option<T> {
    SOME(T),
    NONE,
}

function classify(value: Option<i32>): i32 {
    return match (value) {
        Option::SOME(x) if x > 0 -> x,
        Option::SOME(_) -> 0,
        Option::NONE -> -1,
    };
}
```

Patterns match literals, tuples, enum variants with their payloads, atoms, and `_` for the rest.
Alternatives are written with `|`.

```ignis
function describe(state: atom): i32 {
    return match (state) {
        :ready -> 1,
        :error -> -1,
        _ -> 0,
    };
}
```

## if let

A `let PATTERN = EXPR` can stand where a condition goes, and it can be chained with ordinary
conditions.

```ignis
@lang(try)
enum Option<T> {
    SOME(T),
    NONE,
}

function maybeValue(): Option<i32> {
    return Option::SOME(42);
}

function read(): i32 {
    if (let Option::SOME(v) = maybeValue()) {
        return v;
    }

    return 0;
}

function readLarge(): i32 {
    if (let Option::SOME(x) = maybeValue() && x > 10) {
        return x;
    }

    return 0;
}
```

## while let

The loop runs as long as the pattern keeps matching.

```ignis
@lang(try)
enum Option<T> {
    SOME(T),
    NONE,
}

function nextValue(): Option<i32> {
    return Option::NONE;
}

function total(): i32 {
    let mut sum: i32 = 0;

    while (let Option::SOME(v) = nextValue()) {
        sum += v;
    }

    return sum;
}
```

## let else

`let else` binds on the success path and hands the failure path to a block that must diverge —
`return`, `break`, `continue`, `@panic`, anything that does not fall through.

```ignis
@lang(try)
enum Option<T> {
    SOME(T),
    NONE,
}

function maybeValue(): Option<i32> {
    return Option::SOME(7);
}

function read(): i32 {
    let Option::SOME(v) = maybeValue() else {
        return -1;
    };

    return v;
}
```

The shorthand without a pattern is try-aware sugar: the value must be a `@lang(try)` enum, the name
binds the success payload, and the failure variant jumps to the `else` block. Using it on a value
that is not try-capable reports `A0187`.
