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

## Moving closures

A closure whose environment has a drop function owns that environment. That is the case for a
closure that escapes, whose environment lives on the heap, and for a closure that captures a value
needing a drop by value. Such a closure is not copyable: binding it to another name, assigning it,
storing it in a field or another aggregate, passing it to a parameter, returning it, or producing it
from a branch moves it, and the old name cannot be used afterwards. A closure without a drop function,
such as a non-escaping one that only captures an `i32` by value, stays copyable.

```ignis
record Pair {
    public left: (i32) -> i32;
    public right: (i32) -> i32;
}

function makePair(base: i32): Pair {
    let add = (x: i32): i32 -> x + base;
    let addAgain = (x: i32): i32 -> x + base;

    // `Pair { left: add, right: add }` is an error: the second field would use a moved value.
    return Pair { left: add, right: addAgain };
}

function main(): i32 {
    let pair = makePair(20);
    let left = pair.left;
    let right = pair.right;

    return left(1) + right(1);
}
```

Whoever holds the closure last drops it, which frees its environment and drops what it captured by
value. A record or enum holding a closure drops it with the rest of its contents. Calling a closure
does not move it: `add(1)` can be followed by `add(2)`.

A closure stored in a field moves out of that field when it is handed on by value from a record the
function owns. Read through a `&` or `&mut` reference, the same move is rejected as a move out of a
borrowed value.

A `@noescape` parameter borrows the closure instead of taking it. The caller still owns the closure
after the call, and the callee may call it but not move it anywhere else.

```ignis
function apply(@noescape f: (i32) -> i32, x: i32): i32 {
    return f(x);
}

function makeAdder(base: i32): (i32) -> i32 {
    return (x: i32): i32 -> x + base;
}

function main(): i32 {
    let add = makeAdder(1);

    return apply(add, apply(add, 40));
}
```

A closure body cannot move a value it captured by value out of its environment, as in
`consume(@move held)` for a `held` that needs a drop, or by returning it or destructuring it in a
`match`, `if let` or `let else`. The closure can be called again, and the next call would find the
value gone. Read or borrow it inside the body instead.

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
