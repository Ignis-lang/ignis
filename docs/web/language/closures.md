---
title: Closures
description: Anonymous functions, the capture modes the compiler infers, and what @noescape buys you.
section: language
order: 8
status: stable
---

A closure is an anonymous function that can capture variables from the scope around it. The body is
an expression or a block.

```ignis
function main(): i32 {
    let add = (a: i32, b: i32): i32 -> a + b;
    let double = (x: i32): i32 -> x * 2;

    return add(20, double(11));
}
```

They can be stored in variables, declared at module level, and passed as arguments.

```ignis
const add: (i32, i32) -> i32 = (a: i32, b: i32): i32 -> a + b;

function apply(@noescape f: (i32) -> i32, x: i32): i32 {
    return f(x);
}

function main(): i32 {
    return apply((n: i32): i32 -> n * 2, 21);
}
```

## Capture modes

The mode is inferred from how the variable is used inside the closure.

| Use inside the closure | Mode | Effect |
| --- | --- | --- |
| Read only, copyable type | By value | A copy taken when the closure is created |
| Read only, non-copyable type | Shared reference | A pointer into the enclosing scope |
| Mutated | Mutable reference | A mutable pointer into the enclosing scope |
| Moved | By value | Ownership transferred when the closure is created |

Three builtins override the inference from inside the body: `@move` forces a by-value snapshot,
`@ref` a shared reference, `@refMut` a mutable one. The snapshot matters more than it looks — a
`@move` capture reads the value as it was at creation, not as it is at call time.

## Escaping

A closure that captures by reference and then outlives the scope it captured from would dangle. The
compiler refuses that: storing such a closure in a field, returning it, or passing it to a parameter
that is not marked `@noescape` is an error.

`@noescape` on a parameter is the promise that the closure will not outlive the call, which is what
lets the capture stay a pointer instead of a heap allocation.

```ignis
function forEach(data: *i32, len: i32, @noescape f: (i32) -> void): void {
    let mut i: i32 = 0;

    while (i < len) {
        f(data[i as u64]);
        i = i + 1;
    }

    return;
}

function main(): i32 {
    let arr: i32[3] = [10, 20, 12];
    let mut sum: i32 = 0;

    forEach((&arr[0]) as *i32, 3, (x: i32): void -> { sum = sum + x; });

    return sum;
}
```

Non-escaping closures keep their environment on the stack. Escaping ones get a heap-allocated
environment and a drop function, which is the cost you are agreeing to when you let one escape.
