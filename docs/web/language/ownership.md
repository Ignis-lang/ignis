---
title: Ownership and borrowing
description: Moves, copies, clones, drops and the exclusivity rules the borrow checker enforces.
section: language
order: 7
status: stable
---

Ignis tracks ownership of values that need cleanup — anything carrying `@implements(Drop)`, or
containing something that does. Those values move by default, and using one after it has been
moved, dropped or freed is a compile-time error. There is no garbage collector and no reference
counting behind this: the checks happen before any C is emitted.

## Moves

```ignis
@implements(Drop)
record Resource {
    public id: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let r = Resource { id: 1 };
    let r2 = r;

    return r2.id;
}
```

After `let r2 = r`, reading `r.id` is an error. Passing a non-copy value to a function moves it the
same way. Assigning to a moved variable makes it valid again:

```ignis
@implements(Drop)
record Resource {
    public id: i32;

    drop(&mut self): void {
        return;
    }
}

function main(): i32 {
    let mut r = Resource { id: 1 };
    let r2 = r;

    r = Resource { id: 2 };

    return r.id + r2.id;
}
```

## Copy

Primitives — integers, floats, `boolean`, `char`, pointers, references — are copied, and the
original stays valid. Records and enums are copyable structurally when every field or payload is
recursively copyable, with no annotation needed.

```ignis
record Vec2 {
    public x: f32;
    public y: f32;
}

function main(): i32 {
    let a = Vec2 { x: 1.0, y: 2.0 };
    let b = a;

    return 0;
}
```

Writing `@implements(Copy)` asks the compiler to verify that claim. A type with `Drop` is never
copyable, however primitive its fields look.

## Clone

`@implements(Clone)` requires `clone(&self): Self`. Calling it produces an independent value and
leaves the original where it is.

```ignis
@implements(Drop, Clone)
record Buffer {
    public len: i32;

    drop(&mut self): void {
        return;
    }

    clone(&self): Buffer {
        return Buffer { len: self.len };
    }
}

function main(): i32 {
    let a = Buffer { len: 10 };
    let b = a.clone();

    return a.len + b.len;
}
```

## Drop

A type with `@implements(Drop)` must provide `drop(&mut self): void`. The compiler inserts the call
at every exit: the end of a scope, an early `return`, a `break` or `continue`, and before a live
variable is overwritten. Double drops and use-after-drop are compile-time errors, with a runtime
guard behind them as a second line of defence.

## Borrowing

References follow exclusivity:

- Any number of `&T` may exist at once.
- A `&mut T` is exclusive. No other reference to that value may exist while it lives.
- Mutating a variable while a borrow of it is active is an error.
- Returning a reference to a local is an error.

```ignis
function main(): i32 {
    let mut x: i32 = 10;
    let r1 = &x;
    let r2 = &x;

    return *r1 + *r2;
}
```

## Across the FFI boundary

An extern function does not take ownership of what you hand it, unless the declaration says so with
`@takes`.

```ignis
extern rt {
    function release(@takes ptr: *mut void): void;
}
```

Without it, passing an owned non-copy value into an extern function warns about a likely leak — the
compiler cannot see what the C side does with the pointer, so it tells you instead of guessing.
