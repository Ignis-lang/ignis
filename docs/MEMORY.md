# Memory Management

Ignis uses a compile-time ownership model inspired by Rust, with automatic drop glue, structural Copy derivation, and borrow checking. There is no garbage collector -- the compiler inserts all allocation and deallocation code at compile time.

## Ownership

Every value in Ignis has exactly one owner. When the owner goes out of scope, the value is dropped (cleaned up). Ownership can be transferred via assignment or function calls, but never duplicated unless the type is Copy or explicitly cloned.

```ignis
function main(): void {
    let name: str = "hello";  // main owns `name`
    let other: str = name;    // ownership moves to `other`
    // `name` is no longer valid here
    return;
    // `other` is dropped at end of scope
}
```

## Copy Types

Copy types are duplicated instead of moved on assignment. The original remains valid after the copy.

### Primitive Copy

All primitive types are Copy by default:

- Integers: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`
- Floats: `f32`, `f64`
- `boolean`, `char`, `void`
- Pointers: `*T`, `*mut T` (always Copy regardless of inner type)
- References: `&T`, `&mut T` (always Copy)
- Function types

```ignis
function main(): i32 {
    let x: i32 = 42;
    let y: i32 = x;   // copy, not move
    return x + y;      // both are valid -- returns 84
}
```

### Structural Copy Derivation

Records are automatically Copy if all their fields are Copy. No annotation needed.

```ignis
record Point {
    public x: i32;
    public y: i32;
}

function main(): i32 {
    let p: Point = Point { x: 10, y: 32 };
    let q: Point = p;       // structural copy (all fields are i32, which is Copy)
    return p.x + q.y;       // both valid -- returns 42
}
```

The derivation is transitive: nested records are Copy if all their fields are, all the way down.

```ignis
record Vec2 {
    public x: i32;
    public y: i32;
}

record Rect {
    public origin: Vec2;
    public size: Vec2;
}

// Rect is Copy because Vec2 is Copy, because i32 is Copy
```

A record with any non-Copy field (like `str`) is **not** Copy:

```ignis
record Named {
    public name: str;  // str is not Copy (owns heap data)
    public value: i32;
}

function main(): void {
    let a: Named = Named { name: "hello", value: 42 };
    let b: Named = a;    // move, not copy
    // `a` is no longer valid
    return;
}
```

### Explicit Copy with `@implements(Copy)`

You can declare Copy explicitly with `@implements(Copy)`:

```ignis
@implements(Copy)
record Handle {
    public id: i32;
}
```

The declaration is still validated structurally:

- For records, every field must be Copy
- For enums, every variant payload must be Copy

Copy and Drop are mutually exclusive -- a type cannot be both.

### Fixed-Size Arrays and Tuples

- `T[N]` is Copy if `T` is Copy
- Tuples are Copy if all element types are Copy

## Non-Copy Types

These types own resources and are moved (not copied) on assignment:

- `str` -- owns a heap-allocated character buffer
- Dynamic vectors `[T]` -- own a heap-allocated element buffer
- `Rc<T>` -- shared ownership handle with retain/release semantics
- Records with any non-Copy field
- Records annotated with `@implements(Drop)`
- Enums annotated with `@implements(Drop)`

### Shared Ownership with `Rc<T>`

`Rc<T>` is provided by `std::rc` as a normal library type.

- `let b = a` moves the handle
- `a.clone()` retains and creates a second handle
- `a.strongCount()` returns the current strong reference count
- `a.get()` returns `&T`
- `a.getMut()` returns `*mut T` when `strongCount == 1`, otherwise `null`

`getMut` currently returns a raw pointer. With pattern matching (`match`, `if let`) now available, this could be changed to return `Option<&mut T>` in the future.

## Drop

When a non-Copy value goes out of scope, the compiler automatically inserts cleanup code ("drop glue"). The drop order is reverse declaration order within a scope.

### Automatic Drop Glue

For types without an explicit `drop` method, the compiler generates field-by-field drops recursively:

```ignis
record Person {
    public first: str;
    public last: str;
    public age: i32;
}

function main(): void {
    let p: Person = Person { first: "John", last: "Doe", age: 30 };
    return;
    // compiler inserts: drop(p.last), then drop(p.first)
    // age is i32 (Copy), no drop needed
}
```

### Explicit Drop with `@implements(Drop)`

Records can define a custom `drop` method by declaring `@implements(Drop)`:

```ignis
@implements(Drop)
record Resource {
    public handle: i32;

    drop(&mut self): void {
        // custom cleanup logic
        return;
    }
}
```

Requirements for the `drop` method:
- Signature must be `drop(&mut self): void`
- Must be an instance method with mutable self
- Supported on records and enums

When the compiler inserts a drop for this type, it calls the user-defined `drop` method instead of generating field-by-field glue.

### Drop Scheduling

The compiler schedules drops at three kinds of program points:

1. **Scope end**: When a block `{ ... }` ends, all owned variables declared in that block are dropped in reverse order.

2. **Early exits**: When `return`, `break`, or `continue` exits a scope, all owned variables in the abandoned scopes are dropped before the exit.

3. **Reassignment**: When an owned variable is reassigned, the old value is dropped before the new value is stored.

```ignis
function example(): void {
    let mut s: str = "first";
    s = "second";              // "first" is dropped here
    if (true) {
        let inner: str = "temporary";
        return;                // "temporary" dropped, then "second" dropped
    }
    // "second" would be dropped here in the normal path
}
```

### What Needs Dropping

The `needs_drop` analysis is transitive:

| Type | Needs drop? |
|------|:-----------:|
| Primitives, pointers, references | No |
| `str` | Yes (always) |
| Dynamic `[T]` | Yes (always) |
| Record with `@implements(Drop)` | Yes |
| Record with any field that needs drop | Yes |
| Enum with any payload that needs drop | Yes |
| Tuple with any element that needs drop | Yes |
| `T[N]` where T needs drop | Yes |

## Borrow Checking

Ignis enforces borrow rules at compile time to prevent use-after-move and aliasing violations.

### References

References borrow a value without taking ownership:

```ignis
function length(s: &str): i32 {
    // s borrows the str, does not own it
    return 5;
}

function main(): void {
    let name: str = "hello";
    let len: i32 = length(&name);  // borrow, name stays valid
    return;
}
```

Mutable references allow modification:

```ignis
function increment(x: &mut i32): void {
    *x = *x + 1;
    return;
}

function main(): void {
    let mut value: i32 = 41;
    increment(&mut value);
    // value is now 42
    return;
}
```

### Borrow Rules

The compiler enforces these rules at each program point:

1. **Multiple immutable borrows** are allowed simultaneously
2. **One mutable borrow** is exclusive -- no other borrows (mutable or immutable) can exist at the same time
3. **Immutable-to-mutable upgrade** is forbidden while immutable borrows exist

```ignis
function main(): void {
    let mut x: i32 = 10;
    let a: &i32 = &x;         // immutable borrow
    let b: &i32 = &x;         // second immutable borrow (ok)
    // let c: &mut i32 = &mut x;  // ERROR: mutable borrow while immutable borrows exist
    return;
}
```

Borrows are released at the end of their enclosing block scope.

## Raw Pointers

Raw pointers (`*T`, `*mut T`) bypass borrow checking entirely. They are always Copy and carry no ownership semantics.

```ignis
function main(): i32 {
    let mut value: i32 = 41;
    let ptr: *mut i32 = (&mut value) as *mut i32;
    *ptr = *ptr + 1;
    return value;  // 42
}
```

Pointer operations:

| Operation | Syntax | Description |
|-----------|--------|-------------|
| Address-of | `(&expr) as *T` | Create pointer from reference |
| Dereference | `*ptr` | Read/write through pointer |
| Arithmetic | `ptr + N` | Offset pointer by N elements |
| Cast | `@pointerCast<*U>(ptr)` | Convert between pointer types |
| To integer | `@integerFromPointer(ptr)` | Get numeric address |
| From integer | `@pointerFromInteger<*T>(addr)` | Create pointer from address |

## Copy vs Drop Interaction

Copy and Drop are mutually exclusive. The compiler enforces this:

```ignis
@implements(Copy)
@implements(Drop)
record Bad {              // ERROR: Copy and Drop cannot coexist
    public value: i32;
}
```

The reasoning: a Copy type is duplicated on assignment, so there's no single owner to drop. If cleanup is needed, the type must be non-Copy so ownership is tracked.

### Decision Flowchart

For a record type `R`:

1. If `@implements(Drop)` -> non-Copy, needs drop, uses custom drop method
2. If `@implements(Copy)` -> Copy, no drop
3. If all fields are Copy -> structurally Copy, no drop needed
4. If any field needs drop -> non-Copy, compiler generates drop glue
5. Otherwise -> Copy by default

## Diagnostics

| Code | Message | Severity |
|------|---------|----------|
| A0076 | Cannot call mutating method on immutable variable | Error |
| A0130 | Unknown lang trait | Error |
| A0131 | Drop and Copy cannot coexist | Error |
| A0132 | Missing required method for lang trait | Error |
| A0133 | Invalid signature for lang trait method | Error |
| A0134 | Lang trait not applicable to this type | Error |
| A0148 | Type declares `@implements(Copy)` but a record field is non-Copy | Error |
| A0149 | Type declares `@implements(Copy)` but an enum payload is non-Copy | Error |
