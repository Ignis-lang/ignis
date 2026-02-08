# Traits and Extension Methods

Ignis has a lightweight trait system for core compiler behaviors (Copy, Drop, Clone) and an extension method system for adding methods to existing types.

## Lang Traits

Lang traits are built-in traits that the compiler understands and uses for code generation. They are declared on records with the `@implements` attribute.

### Available Lang Traits

| Trait | Required method | Effect |
|-------|----------------|--------|
| `Drop` | `drop(&mut self): void` | Custom cleanup when value goes out of scope |
| `Clone` | `clone(&self): Self` | Explicit duplication via `.clone()` |
| `Copy` | *(none)* | Value is copied (not moved) on assignment |

Lang traits are not user-extensible -- you cannot define new traits. They exist solely to hook into the compiler's ownership and copy semantics.

### `@implements(Drop)`

Declares that a record has custom cleanup behavior. The record must define a `drop` instance method.

```ignis
@implements(Drop)
record FileHandle {
    public fd: i32;

    drop(&mut self): void {
        // cleanup: close file descriptor
        return;
    }
}
```

Rules:
- The `drop` method signature must be exactly `drop(&mut self): void`
- Only records can implement Drop (not enums)
- A type with Drop is automatically non-Copy
- Drop and Copy cannot coexist on the same type

### `@implements(Clone)`

Declares that a record can be explicitly duplicated. The record must define a `clone` instance method.

```ignis
@implements(Clone)
record Buffer {
    public data: string;
    public size: i32;

    clone(&self): Buffer {
        return Buffer { data: self.data, size: self.size };
    }
}

function main(): void {
    let a: Buffer = Buffer { data: "hello", size: 5 };
    let b: Buffer = a.clone();  // explicit duplicate
    return;
}
```

### `@implements(Copy)`

Forces a record to have copy semantics. No method is required -- this is a flag that tells the compiler to duplicate the value on assignment instead of moving it.

```ignis
@implements(Copy)
record Pair {
    public x: i32;
    public y: i32;
}
```

In practice, `@implements(Copy)` is rarely needed because the compiler already derives Copy structurally for records where all fields are Copy. The explicit annotation is for cases where you want to make the intent clear or override the compiler's analysis.

---

## Record Methods

Records can define instance methods and static methods.

### Instance Methods

Instance methods take `&self` (immutable) or `&mut self` (mutable) as their implicit first parameter.

```ignis
record Counter {
    public value: i32;

    function get(&self): i32 {
        return self.value;
    }

    function increment(&mut self): void {
        self.value = self.value + 1;
        return;
    }
}

function main(): void {
    let mut c: Counter = Counter { value: 0 };
    c.increment();
    let v: i32 = c.get();  // 1
    return;
}
```

The compiler automatically takes a reference to the receiver when calling instance methods -- you write `c.increment()`, not `(&mut c).increment()`.

### Mutability Rules

- Calling a `&mut self` method requires the receiver to be declared `let mut`
- Calling a `&self` method works on both mutable and immutable variables

```ignis
function main(): void {
    let c: Counter = Counter { value: 0 };
    let v: i32 = c.get();       // ok: &self on immutable
    // c.increment();            // ERROR: &mut self on immutable variable
    return;
}
```

---

## Extension Methods

Extension methods add methods to types you don't own (including primitives) without modifying their definition. They are declared as standalone functions with the `@extension` attribute.

### Syntax

```ignis
@extension(TypeName)
function methodName(receiver: TypeName, ...args): ReturnType {
    // body
}
```

The first parameter is the receiver (the value the method is called on). Additional parameters become the method arguments.

### Basic Example

```ignis
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function main(): i32 {
    let x: i32 = 21;
    return x.doubled();  // 42
}
```

The call `x.doubled()` desugars to `doubled(x)`. The compiler resolves the extension method based on the receiver type.

### Extension Methods with Arguments

```ignis
@extension(i32)
function add(value: i32, other: i32): i32 {
    return value + other;
}

function main(): i32 {
    let x: i32 = 30;
    return x.add(12);  // 42
}
```

### Mutable Extensions

The `@extension(TypeName, mut)` form requires the receiver to be mutable:

```ignis
@extension(i32, mut)
function addSelf(value: i32): i32 {
    return value + value;
}

function main(): i32 {
    let mut x: i32 = 21;
    return x.addSelf();  // 42
}
```

Calling a `mut` extension on an immutable variable is a compile error:

```ignis
function main(): void {
    let x: i32 = 5;
    x.addSelf();  // ERROR: Cannot call mutating method 'addSelf' on immutable variable 'x'
    return;
}
```

### Supported Target Types

Extension methods can be defined on primitive types:

- Integer types: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`
- Float types: `f32`, `f64`
- `boolean`, `char`, `string`

Attempting to extend a user-defined record or an unknown type is a compile error.

### Receiver Restrictions

Extension methods can only be called on **variables**, **field accesses**, and **index accesses**. They cannot be called on literals or temporary expressions.

```ignis
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function getNumber(): i32 { return 5; }

function main(): void {
    let x: i32 = 10;

    x.doubled();              // ok: variable
    // 10.doubled();          // ERROR: cannot call on literal
    // (x + 1).doubled();    // ERROR: cannot call on temporary
    // getNumber().doubled(); // ERROR: cannot call on temporary
    return;
}
```

To use an extension method on a computed value, assign it to a variable first:

```ignis
function main(): i32 {
    let x: i32 = 10;
    let sum: i32 = x + 1;
    return sum.doubled();     // ok
}
```

### Multiple Extensions on the Same Type

Multiple extension methods can be defined for the same type. They are resolved by name:

```ignis
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

@extension(i32)
function isPositive(value: i32): boolean {
    return value > 0;
}

function main(): void {
    let x: i32 = 21;
    let d: i32 = x.doubled();
    let p: boolean = x.isPositive();
    return;
}
```

### Chaining via Variables

Since extension methods on temporaries are forbidden, chaining requires intermediate variables:

```ignis
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

function main(): i32 {
    let x: i32 = 7;
    let y: i32 = x.doubled();    // 14
    return y.doubled();           // 28
}
```

### Method Resolution Priority

When a record has both an instance method and an extension method with the same name, the instance method wins. Extension methods only apply when no native method is found on the type.

### Standard Library Extensions

The standard library defines extension methods on primitive types in prelude modules (`string`, `number`, `vector`, `types`). These are automatically loaded when `std = true` in the project configuration.

Examples from the standard library:
- `i32.toString()`, `f64.toString()`, `boolean.toString()`
- `i32.abs()`, `f64.abs()`
- String and vector utility methods

When `std = false` (standalone mode), prelude modules are not loaded and these methods are unavailable.

## Attributes Reference

### Record Attributes

| Attribute | Description |
|-----------|-------------|
| `@packed` | Remove field padding (tightly packed layout) |
| `@aligned(N)` | Set minimum alignment to N bytes |
| `@implements(Drop)` | Declare custom drop behavior |
| `@implements(Clone)` | Declare explicit clone method |
| `@implements(Copy)` | Force copy semantics |

### Function Attributes

| Attribute | Description |
|-----------|-------------|
| `@externName("name")` | Set the C symbol name for FFI |
| `@cold` | Hint that this function is rarely called |
| `@deprecated` | Mark function as deprecated (emits warning on use) |
| `@deprecated("message")` | Deprecated with a custom message |
| `@extension(type)` | Define extension method for a type |
| `@extension(type, mut)` | Define mutable extension method |
| `@inline` | Suggest inlining |
| `@inline(always)` | Force inlining |
| `@inline(never)` | Prevent inlining |

### Lint Directives

| Attribute | Description |
|-----------|-------------|
| `@allow(lint_name)` | Suppress a lint warning |
| `@warn(lint_name)` | Promote a lint to warning level |
| `@deny(lint_name)` | Promote a lint to error level |

Available lints: `unused_variable`, `unused_import`, `deprecated`.

### Field Attributes

| Attribute | Description |
|-----------|-------------|
| `@aligned(N)` | Set minimum alignment for this field |

## Diagnostics

| Code | Message | Severity |
|------|---------|----------|
| A0076 | Cannot call mutating method on immutable variable | Error |
| A0130 | Unknown lang trait 'Name' | Error |
| A0131 | Missing required method for lang trait | Error |
| A0132 | Drop and Copy cannot coexist on same type | Error |
| A0133 | Invalid signature for lang trait method | Error |
| A0134 | Lang trait not applicable to this type | Error |
| A0135 | Invalid target type for @extension | Error |
| A0136 | @extension function requires at least one parameter | Error |
| A0137 | Extension receiver type mismatch | Error |
| A0138 | Cannot call extension method on literal | Error |
| A0139 | Cannot call extension method on temporary | Error |
