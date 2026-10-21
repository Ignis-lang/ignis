# Traits and Extension Methods

Ignis now has two trait layers:

1. **Lang traits** (`Drop`, `Clone`, `Copy`, `Send`, `Sync`) used by ownership, codegen and thread-safety checks.
2. **User-defined traits** (`trait Name { ... }`) used as method contracts on records and, for canonical equality, enums.

It also has an extension method system (`@extension`) for adding methods to existing types.

## Lang Traits

Lang traits are built-in traits that the compiler understands and uses for code generation. They are declared with `@implements(...)` on records and enums.

### Available Lang Traits

| Trait | Required method | Effect |
|-------|----------------|--------|
| `Drop` | `drop(&mut self): void` | Custom cleanup when value goes out of scope |
| `Clone` | `clone(&self): Self` | Explicit duplication via `.clone()` |
| `Copy` | *(none)* | Value is copied (not moved) on assignment |
| `Send` | *(none)* | Value may move to another thread |
| `Sync` | *(none)* | Value may be shared between threads through `&` |

Lang traits are fixed and compiler-defined. User-defined traits are a separate feature covered below.

### `@implements(Drop)`

Declares custom cleanup behavior. The type must define a `drop` instance method.

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
- Records and enums can implement `Drop`
- A type with Drop is automatically non-Copy
- Drop and Copy cannot coexist on the same type

### `@implements(Clone)`

Declares that a type can be explicitly duplicated. The type must define a `clone` instance method.

```ignis
@implements(Clone)
record Buffer {
    public data: str;
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

Declares copy semantics for a type. No method is required.

```ignis
@implements(Copy)
record Pair {
    public x: i32;
    public y: i32;
}
```

`@implements(Copy)` is validated structurally. For records, every field must be Copy. For enums, every variant payload must be Copy. If any member is non-Copy, compilation fails.

---

## User-Defined Traits

User-defined traits describe behavior that records must provide.

### Canonical `Eq`

The canonical equality contract used by generic test assertions and builtin
`@eq<T>` dispatch is `std::hash::Eq`.

```ignis
import Eq from "std::hash";

@implements(Eq)
record UserId {
    public value: i32;

    equals(&self, other: &UserId): boolean {
        return self.value == other.value;
    }
}
```

Rules:

- `Eq` is implemented with `@implements(Eq)`.
- The required method is `equals(&self, other: &ConcreteType): boolean`.
- Records and enums can implement canonical `Eq`.
- Generic `std::test::Test::assertEq<T>` / `assertNe<T>` and builtin `@eq<T>` rely on this contract.
- Unsupported equality must be rejected during analysis before code generation.

Compile-time directives that want to derive trait conformance must target the current implementation model: internal attached methods plus implemented-trait metadata.

Directive execution does not yet synthesize `Eq` methods for you, so the runtime equality contract remains handwritten today.

### Declaring a Trait

```ignis
trait Describable {
    describe(&self): i32;

    code(&self): i32 {
        return 0;
    }
}
```

Rules:
- Trait methods use the same style as record/enum methods: `name(...)` (no `function` required).
- A method ending with `;` is required (no body).
- A method with `{ ... }` is a default method.
- Trait methods must be instance methods (`&self` or `&mut self`).
- Static methods are not allowed in traits.
- Fields are not allowed in traits.
- Traits are allowed at top level and inside `namespace` blocks.
- Traits are not allowed inside `extern` blocks.

### Implementing a Trait

Use `@implements(TraitName)` on a record:

```ignis
trait Greetable {
    greet(&self): i32;
}

@implements(Greetable)
record Person {
    public age: i32;

    greet(&self): i32 {
        return self.age;
    }
}
```

You can implement multiple traits either by repeating the attribute or by passing multiple names in one `@implements(...)`.

Each name resolves to a trait the way a type name resolves to a type: a qualified path such as
`Shapes::Measured` names the trait declared under that path, and a bare name is looked up in the
enclosing namespaces (innermost first), then among the module's declarations and its imports. A
trait has to be in scope like any type: a trait declared in another namespace or module is not
found by its bare name, even when it is the only trait of that name. A name that resolves to no
trait, or to a declaration that is not a trait, is an error (A0142). The language traits are the
exception: the lang traits (`Drop`, `Clone`, `Copy`, `Send`, `Sync`) and `Eq` and `Hash` need no
declaration in scope and are recognized by name.

### Default Methods

If a record does not define a method that has a default body in the trait, the compiler uses that default body for the record.

```ignis
trait HasValue {
    getValue(&self): i32;

    doubled(&self): i32 {
        return self.getValue() * 2;
    }
}

@implements(HasValue)
record Box {
    public value: i32;

    getValue(&self): i32 {
        return self.value;
    }
}
```

### Signature Validation

When a record method implements a trait method, the signature must match exactly:

- `&self` vs `&mut self`
- Number of explicit parameters
- Parameter types
- Return type

If a required trait method is missing, compilation fails (A0140), whether the trait is named bare or
by its path. A method with a default body in the trait is not required. Each named trait is checked
on its own, so a complete `A::Named` does not excuse a missing method of `B::Named`.

### `Send` and `Sync`

`Send` and `Sync` need no declaration or import: they are accepted in `@implements` and as bounds.
The compiler answers them from a type's structure:

| Type | `Send` | `Sync` |
|------|--------|--------|
| Integer and float primitives, `boolean`, `char`, `str`, `void`, `never`, atoms, ranges, raw C function pointers | yes | yes |
| `*T`, `*mut T` | no | no |
| `&T`, slices `T[]` | when `T` is `Sync` | when `T` is `Sync` |
| `&mut T` | when `T` is `Send` | when `T` is `Sync` |
| Fixed arrays, tuples | when every element is | when every element is |
| Records, enums, generic instances | declared, or every field / payload is | declared, or every field / payload is |
| Function values | no; only a call argument is judged, at the call (below) | no; only a call argument is judged, at the call (below) |
| Type parameters | when the parameter declares the bound | when the parameter declares the bound |

A function type does not record what a closure captured, so a function-typed argument bound to a
`Send` or `Sync` type parameter, or passed to a `@sendable` parameter, is judged where it is
passed, once captures are known. Anywhere else, such as a field of `Holder<() -> i32>`, a function
type is neither `Send` nor `Sync`.

- a closure literal: every capture moved in by value has to be `Send` (`Sync` for a `Sync` bound),
  and every capture read by reference has to be `Sync`. For a `Sync` bound, a capture by mutable
  reference is rejected outright: a closure shared between threads may run on several of them at
  once. Otherwise A0252,
- a named function: accepted, it has no environment,
- a `@sendable` parameter of the enclosing function, forwarded as is: accepted, its own caller was
  checked,
- any other function value: rejected (A0253).

#### `@sendable`

A type parameter `F: Send` cannot be called inside the function. `@sendable` on a parameter of
function type gives the same check at every call while keeping the parameter an ordinary,
callable function value. Arguments are judged with the `Send` rules above. `@sendable` on a
parameter that is not of function type is an error (A0255). An alias is judged by the type it names: an alias to a function type is accepted, an alias to anything else is A0255, and so is a generic alias, which is not resolved to a function type. A parameter typed by an alias to a function type is callable like one spelled out. It combines with `@noescape`, which
keeps deciding where by-reference captures are allowed.

```ignis
function spawn(@sendable task: () -> i32): i32 {
  return task();
}

function later(@sendable task: () -> i32): i32 {
  return spawn(task);   // ok: forwarded
}
```

A record or enum can declare `@implements(Send)` or `@implements(Sync)` when its structure does not
prove it, for example when it guards a raw pointer itself. The declaration wins, and the
`thread_safety` lint reports it (A0254, a warning by default) naming the first field or payload that
breaks it. `@allow(thread_safety)` on the record or enum, or on a top-level function for the whole
module, silences it.

On a generic record or enum the declaration speaks only for its own fields: an instance is Send
(or Sync) only when every type argument is too, so `Vector<String>` is Send and
`Vector<*mut i32>` is not. A type that needs a different condition states it as bounds on its own
type parameters, as `Arc<T: Send & Sync>` does. `String`, `Vector<T>` and `HashMap<K, V>` declare
both traits this way; `Rc` and `Weak` declare neither.

```ignis
@allow(thread_safety)
@implements(Send, Sync)
record Handle {
  public raw: *mut i32;
}

function spawn<F: Send>(task: F): void {}

function main(): void {
  let count: i32 = 3;
  spawn((): i32 -> count);   // ok: captures an i32
}
```

The check follows types, not values. A pointer converted to an integer (`pointer as u64`) is an integer, which is Send, so it crosses threads unchecked and turns back into a pointer on the other side. That conversion is the explicit way out of the check, and the code that writes it is responsible for synchronizing every access through the pointer.

A `@threadLocal` static field is never shared between threads: each thread has its own copy, so no Send or Sync check applies to it. It cannot be borrowed (A0261), so no `&T` pointing into one thread's copy can reach another thread.

### Trait Bounds

A type parameter can be bounded by one or more traits, joined with `&`. Every instantiation has to
satisfy every bound, whether the type argument is written or inferred, and whether the generic is
a function, a method, a record or an enum:

- A record or enum satisfies a bound by naming the trait in `@implements`. Having methods with the
  right names is not enough. A trait declared in a namespace can be named by its path
  (`@implements(Shapes::Measured)`) or, where it is in scope, by its bare name. The `@implements`
  argument and the bound both resolve to a trait declaration and are compared as declarations, so
  two traits with the same name in different namespaces or modules stay apart: a type implementing
  `A::Named` does not satisfy `T: B::Named`, and one implementing `std::hash::Hash` does not
  satisfy the `K: Hash` bound of `HashMap`, which names the `Hash` of `std::collections`. A name
  that resolves to no trait is matched by name; only a language trait can, since any other
  unresolved bound is an error (A0142), whether or not the generic is used.
- A type parameter answers the methods of every trait it is bounded by, including their default
  methods. A method declared by more than one of its bounds is ambiguous (A0262).
- A type parameter satisfies a bound only when it declares the same bound itself.
- `Eq` and `Hash` are satisfied by the integer primitives, `boolean`, `char` and `str`, and `Copy`
  by every Copy type, since primitives have no declaration to write `@implements` on.
- `Send` and `Sync` are judged from the type's structure, or taken from an `@implements(Send)` /
  `@implements(Sync)` declaration. A closure bound to a `Send` or `Sync` parameter is judged by
  what it captures (A0252); any other function value is rejected (A0253).

A type argument that misses a bound is an error (A0247). The diagnostic names the trait by the
path it is declared under, prefixed with its module (`std::hash::Hash`,
`std::collections::hash_map::Hash`) when another trait is declared under the same path in another
module. Declared types are checked too, even
when no expression has them: record and static field types, enum payload types, and the parameter
and return types of every function and method, called or not. A generic record or enum's own
type parameter used there satisfies a bound only when it declares the bound itself.

### Current Limit

General user-defined trait implementation checks still primarily target records.
Canonical `Eq` is the current enum exception because generic test assertions and
builtin equality rely on it.

---

## Record Methods

Records can define instance methods and static methods.

### Instance Methods

Instance methods take `&self` (immutable) or `&mut self` (mutable) as their implicit first parameter.

```ignis
record Counter {
    public value: i32;

    get(&self): i32 {
        return self.value;
    }

    increment(&mut self): void {
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
- `boolean`, `char`, `str`

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
| `@implements(Send)` | Declare the type may move between threads |
| `@implements(Sync)` | Declare the type may be shared between threads |
| `@implements(MyTrait)` | Declare a user-defined trait implementation on a record |

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

Available lints: `unused_variable`, `unused_import`, `unused_mut`, `deprecated`, `thread_safety`.

Directives on top-level functions apply to the whole module. `thread_safety` can also be set on the
record or enum it reports, which overrides the module level for that declaration.

### Field Attributes

| Attribute | Description |
|-----------|-------------|
| `@aligned(N)` | Set minimum alignment for this field |

## Diagnostics

| Code | Message | Severity |
|------|---------|----------|
| A0076 | Cannot call mutating method on immutable variable | Error |
| A0130 | Unknown lang trait 'Name' | Error |
| A0131 | Drop and Copy cannot coexist on same type | Error |
| A0132 | Missing required method for lang trait | Error |
| A0133 | Invalid signature for lang trait method | Error |
| A0134 | Lang trait not applicable to this type | Error |
| A0135 | Invalid target type for @extension | Error |
| A0136 | @extension function requires at least one parameter | Error |
| A0137 | Extension receiver type mismatch | Error |
| A0138 | Cannot call extension method on literal | Error |
| A0139 | Cannot call extension method on temporary | Error |
| A0140 | Record or enum names a trait in `@implements` but does not declare one of its required methods | Error |
| A0141 | Method signature does not match trait definition | Error |
| A0142 | Unknown trait in `@implements(...)` or a generic bound | Error |
| A0143 | Traits cannot be declared in `extern` blocks | Error |
| A0144 | Trait method must have `&self` or `&mut self` | Error |
| A0145 | Fields are not allowed in trait declarations | Error |
| A0146 | Static methods are not allowed in trait declarations | Error |
| A0148 | Type declares `@implements(Copy)` but a record field is non-Copy | Error |
| A0149 | Type declares `@implements(Copy)` but an enum payload is non-Copy | Error |
| A0247 | Type argument does not satisfy a bound of its type parameter | Error |
| A0252 | Closure bound to a `Send` / `Sync` parameter captures a value that is not thread-safe | Error |
| A0253 | Function value bound to a `Send` / `Sync` parameter whose captures cannot be known | Error |
| A0254 | `@implements(Send)` / `@implements(Sync)` on a type whose structure does not support it (`thread_safety` lint) | Warning |
| A0255 | `@sendable` on a parameter that is not of function type | Error |
| A0262 | Method called on a type parameter is declared by more than one of its bounds | Error |
