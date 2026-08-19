---
title: Control flow
description: Conditions, loops, defer, and the statements that leave a block.
section: language
order: 9
status: stable
---

Conditions are parenthesized.

```ignis
function sign(x: i32): i32 {
    if (x > 0) {
        return 1;
    } else {
        return 0;
    }
}
```

## Loops

`while` takes a condition. The C-style `for` takes an initializer, a condition and a step, and the
type annotation on the loop variable is optional.

```ignis
function sum(): i32 {
    let mut total: i32 = 0;

    for (let i = 0; i < 10; i++) {
        total += i;
    }

    return total;
}
```

`for of` walks a sequence. Binding the element as a reference avoids copying it.

```ignis
function total(): i32 {
    let arr: i32[3] = [1, 2, 3];
    let mut sum: i32 = 0;

    for (let x of arr) {
        sum += x;
    }

    return sum;
}

function totalByReference(): i32 {
    let arr: i32[3] = [1, 2, 3];
    let mut sum: i32 = 0;

    for (let x: &i32 of arr) {
        sum += *x;
    }

    return sum;
}
```

Pattern-driven loops are covered in [pattern matching](/docs/language/pattern-matching).

## defer

`defer` schedules an expression for the end of the scope. Several defers in one scope run in
reverse order — last registered, first to run — and all of them run before automatic drops.

```ignis
import Io from "std::io";

function example(): void {
    defer Io::println("third");
    defer Io::println("second");
    defer Io::println("first");

    return;
}
```

That prints `first`, `second`, `third`.

Its real use is pairing an acquire with its release next to each other, so the release cannot be
lost down a branch you forgot about:

```ignis
import LibC from "std::libc";

function readFile(path: str): i32 {
    let fd: i32 = LibC::File::open(path, LibC::File::O_RDONLY);

    if (fd == -1) {
        return -1;
    }

    defer LibC::File::close(fd);

    return 0;
}
```

Defers fire at every exit: `return`, `break`, `continue`, and falling off the end of the block. Two
restrictions: the deferred expression must be `void`-typed, and the try operator `!` is not allowed
inside one.

## Leaving a block

```ignis
function example(): i32 {
    let mut i: i32 = 0;

    while (i < 10) {
        if (i == 5) {
            break;
        }

        if (i == 2) {
            i += 1;
            continue;
        }

        i += 1;
    }

    return i;
}
```
