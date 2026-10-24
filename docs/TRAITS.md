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
- An instance method takes `&self` or `&mut self` (A0144 otherwise).
- A method marked `static` takes no receiver and belongs to the implementing type. It can be
  required (`static name(...): R;`) or have a default body. `Self` in its parameter and return
  types stands for the implementing type.
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

A static trait method is declared with `static`:

```ignis
trait Make {
    static make(seed: i32): Self;

    static label(): i32 {
        return 0;
    }
}
```

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

The same holds for a trait declared in another module: the record inherits the default whether
it names the trait directly, through a module that re-exports it, or by a namespaced path. Names
in the inherited body and signature resolve in the trait's module and namespaces, never in the
implementing module: a default may call a private helper of its module, and a declaration of the
same name in the implementing module does not change what the default refers to.

An error in a default that every implementing type hits the same way is reported once, at the
trait's file and line. One that only some implementing type hits is reported once for that type,
still at the trait's line, with a note that names the trait, the method and the inheriting type.

### Signature Validation

When a record or enum names a trait in `@implements`, the compiler checks each trait method:

- A required method has to be declared on the type. If it is missing, compilation fails (A0140),
  whether the trait is named bare or by its path. A method with a default body in the trait is not
  required: the type inherits the default, static defaults included.
- The method has to be declared with the same kind: a static trait method by a `static` method, an
  instance trait method by a method with a `self` parameter (A0276 otherwise).
- A static method has to match the trait's signature: the same number of parameters, the same
  parameter types and the same return type, with `Self` in the trait's signature read as the
  implementing type (A0277). For a generic trait the static signature is not compared.

Instance method signatures are not compared yet: an instance method that implements a trait method
only has to have the same name and take a `self` parameter. A different parameter list or return
type is not reported at the declaration.

Each named trait is checked on its own, so a complete `A::Named` does not excuse a missing method
of `B::Named`.

### `Send` and `Sync`

`Send` and `Sync` need no declaration or import: they are accepted in `@implements` and as bounds.
The compiler answers them from a type's structure:

| Type | `Send` | `Sync` |
|------|--------|--------|
| Integer and float primitives, `boolean`, `char`, `str`, `void`, `never`, atoms, ranges, function pointers `fn(A) -> R` | yes | yes |
| `*T`, `*mut T` | no | no |
| `&T`, slices `T[]` | when `T` is `Sync` | when `T` is `Sync` |
| `&mut T` | when `T` is `Send` | when `T` is `Sync` |
| Fixed arrays, tuples | when every element is | when every element is |
| Records, enums, generic instances | declared, or every field / payload is | declared, or every field / payload is |
| Closure values `(A) -> R` | no; only a call argument is judged, at the call (below) | no; only a call argument is judged, at the call (below) |
| Type parameters | when the parameter declares the bound | when the parameter declares the bound |

A function pointer type `fn(A) -> R` has no environment, so it is always both, everywhere: as a
field, a type argument, a `@sendable` argument or a value moved into another thread. A record whose
fields are function pointers is Send and Sync from its structure, with no declaration. See
[function pointers](web/language/closures.md#function-pointers).

A closure type does not record what a closure captured, so a closure-typed argument bound to a
`Send` or `Sync` type parameter, or passed to a `@sendable` parameter, is judged where it is
passed, once captures are known. Anywhere else, such as a field of `Holder<() -> i32>`, a closure
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

On a generic record or enum the declaration speaks only for its own fields. Each type argument of
an instance is judged by its type parameter:

- a parameter with no `Send` or `Sync` bound: the argument has to be Send for the instance to be
  Send, and Sync for it to be Sync. `Vector<String>` is Send and Sync; `Vector<*mut i32>` is
  neither.
- a parameter bounded by `Send` or `Sync`: the argument has to satisfy exactly those bounds,
  whichever trait the instance is judged for. `Mutex<T: Send>` declares both, so `Mutex<T>` is Send
  and Sync whenever `T` is Send, even when `T` is not Sync; `Arc<T: Send & Sync>` is Send and Sync
  when `T` is both.

A type that needs a condition other than the default states it as bounds on its own type
parameters this way. The `thread_safety` lint takes such a bound at its word: a field of a bounded
parameter's type does not make it warn. `String`, `Vector<T>` and `HashMap<K, V>` declare both
traits with no bound; `Rc`, `Weak` and `MutexGuard` declare neither.

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
- `T::name(args)` calls a static method that a bound of `T` declares. `Self` in that method's
  signature reads as `T`, so a trait method returning `Result<Self, Error>` returns
  `Result<T, Error>` at the call. After monomorphization the call goes to the static method of the
  type `T` stands for, or to the trait's default when the type does not declare it. Only the bounds
  of `T` are searched: a static method of the same name on an unrelated record is never picked,
  and a name no bound declares as static is an error (A0278).
- Inside a trait's default body, static or instance, `Self::name(args)` calls the trait's static
  method `name` on the implementing type, the way `T::name(args)` does on a bounded type
  parameter: an implementer that inherits the default reaches its own `name`, or the trait's
  default for `name` when it declares none. Inside a record or enum, `Self::name` means what the
  owner's own name does; outside a record, enum or trait it is an error (A0280).
- A static trait method is not callable through a value (`value.make()`), neither on a type
  parameter nor on the implementing type, and `Trait::make()` names no implementing type, so it is
  an error (A0279).
- A type parameter satisfies a bound only when it declares the same bound itself.
- `Eq` and `Hash` are satisfied by the integer primitives, `boolean`, `char` and `str`, and `Copy`
  by every Copy type, since primitives have no declaration to write `@implements` on.
- Any other trait is satisfied by a primitive, `str` or a type declared elsewhere through an
  extension block that implements it (see [Implementing a Trait for an Existing
  Type](#implementing-a-trait-for-an-existing-type)). A generic block satisfies the bound only for
  type arguments that satisfy its own bounds: `extension<T: Describe> Option<T>` makes
  `Option<i64>` a `Describe` when `i64` is one, and leaves `Option<boolean>` without it.
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

Overload resolution takes bounds into account. A generic overload whose inferred type arguments
miss a bound is set aside before the overloads are ranked, so the call picks another overload
instead of reporting the bound or a tie. Only when every matching overload misses a bound is the
lowest-cost one selected, and the call reports the bound it misses (A0247). See the language
reference, section 4.1, for how overloads are ranked.

### Implementing a Trait for an Existing Type

A type that cannot carry `@implements` itself, a primitive, `str`, or a record or enum declared
elsewhere such as the standard library's `String` or `Option`, implements a trait through an
extension block:

```ignis
trait Describe {
  describe(&self): i64;

  static read(raw: i64): Self;

  twice(&self): i64 {
    return self.describe() * 2;
  }
}

@implements(Describe)
extension i64 {
  describe(&self): i64 {
    return *self;
  }

  static read(raw: i64): i64 {
    return raw + 1;
  }
}

@implements(Describe)
extension<T: Describe> Option<T> {
  describe(&self): i64 {
    return match (self) {
      Option::SOME(inner) -> inner.describe(),
      Option::NONE -> -1,
    };
  }

  static read(raw: i64): Option<T> {
    return Option::SOME(T::read(raw));
  }
}
```

Rules:

- The block declares methods only, following record-method rules: an instance method takes
  `&self` or `&mut self`, a static one is marked `static`. `Self` is the extended type.
- The methods are checked against the trait like a record's (A0140, A0276, A0277), and the
  trait's defaults are inherited, from another module too.
- They are called on the type itself, `value.describe()` and `(5 as i64).describe()`, and a
  static one by the type's path, `i64::read(3)`. A method the type declares itself wins over one
  of the same name in a block, and an `@extension(T)` function wins over a block too.
- The type satisfies the trait as a bound, and `T::read` with `T = i64` reaches the block's static
  method after monomorphization.
- A generic block extends an instance over exactly its own type parameters, in order. It applies
  only where the type arguments satisfy its bounds; elsewhere the method is not found (A0054) and
  the bound is not met (A0247).
- `extension` is a contextual keyword: an identifier named `extension` keeps working.

| Code | Condition |
|------|-----------|
| A0285 | The block names no trait in `@implements` |
| A0286 | The block is declared outside the module of the trait and of the type; a primitive is extended only in the trait's module |
| A0287 | The type implements the trait twice: two blocks, or a block and the type's own `@implements`. Both spans are named |
| A0288 | The block names a lang trait (`Drop`, `Clone`, `Copy`, `Send`, `Sync`) or the canonical `Eq` or `Hash`, which the compiler answers from the type itself |
| A0289 | The target is not a primitive, `str`, a record or an enum, or a generic instance not over the block's own parameters |

Per-function `@extension(type)` (below) is unchanged.

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

### Declarable Attributes

A program declares its own attributes as records marked `@attribute(<targets>)`. A declarable
attribute is written with a capitalized name and resolves like a type name: through the enclosing
namespaces, the module's own declarations and its imports, or by a qualified path.

```ignis
@attribute(record)
export record Table {
  public name: str;
}

export namespace Orm {
  @attribute(field)
  export record PrimaryKey {}
}
```

```ignis
import Table, Orm from "./orm";

@Table("tasks")
record Task {
  @Orm::PrimaryKey
  public id: i64;
}
```

| Rule | Detail |
|------|--------|
| Targets | One or more of `record`, `enum`, `field`, `function` (a method counts) and `variant`. |
| Arguments | One per field of the record, in declaration order. A record with no fields is written without parentheses. |
| Argument values | Compile-time constants: literals, constants, negation and what constant evaluation folds. Each must fit its field: an integer in the field type's range, a float in `f32`/`f64`, a string in `str`, plus `boolean` and `char`. |
| Ordinary use | An attribute record is an ordinary record: it can be constructed and passed like any other. |
| Built-ins | Lowercase attributes (`@packed`, `@implements(...)`, ...) keep their meaning and are not records. |

| Code | Message | Severity |
|------|---------|----------|
| A0117 | Unknown attribute; for a declarable one in another module, the message names the import to add | Error |
| A0118 | Wrong number of attribute arguments | Error |
| A0281 | Declarable attribute used on a target its `@attribute(...)` does not list | Error |
| A0282 | Capitalized attribute names something other than a record marked `@attribute` | Error |
| A0283 | Attribute argument does not fit its field | Error |
| A0284 | Attribute argument is not a compile-time constant | Error |
| A0290 | `@attribute` without targets, or naming an unknown target | Error |

## Field Reflection: `comptime for`

A method whose `Self` is a concrete record walks that record's fields with a compile-time loop.
The loop is unrolled before name resolution: one copy of its body per instance field, in declaration
order, so each copy is analyzed with the field's concrete type. A trait default that uses it unrolls
on each implementing record its copy lands on, hand-written record methods and methods of an
`extension` block whose target is a record of the same module unroll over that record.

```ignis
trait Describe {
  describe(&self): String {
    let mut text: String = String::new();

    comptime for (field of @fields<Self>()) {
      if (field.index > 0) {
        text.pushStr(", ");
      }

      text.pushStr(field.name);
      text.pushStr(": ");
      let shown: String = show(@field(self, field));
      text.push(&shown);
    }

    return text;
  }
}
```

Inside the body, `field` is a descriptor, not a value:

| Form | Meaning |
|------|---------|
| `field.name` | The field's name, a `str` literal |
| `field.index` | Its position, an `i32` literal starting at 0 |
| `@field(value, field)` | `value.<name>`: a place, readable, assignable and borrowable |
| `@fieldType(field)` | The field's written type, valid wherever a type is |
| `@attribute<A>(field)` | `Option<A>`: the constant values of declarable attribute `A` on the field, or `NONE` |
| `@attribute<A>(Self)` | The same for the record itself |

A record initializer builds `Self` field by field with one `comptime for` entry, which unrolls into
one `<name>: <value>` entry per field:

```ignis
trait Seeded {
  static seeded(seed: i32): Self {
    return Self {
      comptime for (field of @fields<Self>()) @field(field): (seed + field.index) as @fieldType(field),
    };
  }
}
```

Per-field-type behavior comes from overloads: each copy calls the overload for its field's type, and
a field type with no matching overload is reported at its place in the loop body with a note naming
the field. In the trait's own body `Self` has no known fields, so the loop unrolls over none there.

| Code | Message | Severity |
|------|---------|----------|
| A0292 | `break` or `continue` that would cross a `comptime for` | Error |
| A0293 | `@fields<T>()` over something other than a record `Self`: a type parameter, an enum, another type | Error |
| A0294 | `comptime for` outside a method | Error |
| A0295 | `@field`, `@fieldType`, `@attribute<A>(field)` or `@fields` outside the loop that declares the descriptor | Error |
| A0296 | Unknown descriptor member, or the descriptor used as a value | Error |

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
| A0142 | Unknown trait in `@implements(...)` or a generic bound | Error |
| A0143 | Traits cannot be declared in `extern` blocks | Error |
| A0144 | Trait instance method must have `&self` or `&mut self` | Error |
| A0145 | Fields are not allowed in trait declarations | Error |
| A0148 | Type declares `@implements(Copy)` but a record field is non-Copy | Error |
| A0149 | Type declares `@implements(Copy)` but an enum payload is non-Copy | Error |
| A0247 | Type argument does not satisfy a bound of its type parameter | Error |
| A0252 | Closure bound to a `Send` / `Sync` parameter captures a value that is not thread-safe | Error |
| A0253 | Function value bound to a `Send` / `Sync` parameter whose captures cannot be known | Error |
| A0254 | `@implements(Send)` / `@implements(Sync)` on a type whose structure does not support it (`thread_safety` lint) | Warning |
| A0255 | `@sendable` on a parameter that is not of function type | Error |
| A0262 | Method called on a type parameter is declared by more than one of its bounds | Error |
| A0276 | Trait method implemented with the other kind (static for instance, or instance for static) | Error |
| A0277 | Static method does not match the static trait method's signature, `Self` read as the implementing type | Error |
| A0278 | `T::name(args)` where no bound of `T` declares a static method `name` | Error |
| A0279 | `Trait::name(args)` calls a static trait method without an implementing type | Error |
| A0280 | `Self::name` written outside a record, enum or trait | Error |
| A0285 | `extension` block without a trait in `@implements` | Error |
| A0286 | `extension` block outside the module of its trait and of its type | Error |
| A0287 | A type implements the same trait twice | Error |
| A0288 | `extension` block implementing a lang trait or canonical `Eq`/`Hash` | Error |
| A0289 | Invalid `extension` target type | Error |
| A0292 | `break` or `continue` crossing a `comptime for` | Error |
| A0293 | `@fields` over a type that is not the record `Self` | Error |
| A0294 | `comptime for` outside a method | Error |
| A0295 | Field descriptor builtin outside its `comptime for` | Error |
| A0296 | Unknown descriptor member or descriptor used as a value | Error |
