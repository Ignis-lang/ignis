# Ignis Language Reference (Current)

This document describes the syntax currently implemented by the compiler on `main`.
It focuses on language-level features with short, practical examples.

## 1. Program Structure

An Ignis file is a sequence of declarations.

```ignis
import Io from "std::io";

const VERSION: i32 = 1;

function main(): i32 {
    Io::println("Hello");
    return VERSION;
}
```

Top-level declarations include:

- `function`
- `const`
- `import`
- `export`
- `extern`
- `namespace`
- `type` aliases
- `record`
- `enum`
- `trait`

## 2. Comments and Documentation Comments

`///` documents the next item. `//!` documents the enclosing module or namespace.

```ignis
//! Math utilities module docs
//! this block documents the enclosing file/module

// line comment

/* block comment */

/// doc comment for the next declaration
function add(a: i32, b: i32): i32 {
    return a + b;
}

//! inner doc comment for the enclosing module/namespace
namespace Math {
    //! docs for namespace body
    function id(x: i32): i32 {
        return x;
    }
}
```

## 3. Types

### 3.1 Primitive Types

- Signed integers: `i8`, `i16`, `i32`, `i64`
- Unsigned integers: `u8`, `u16`, `u32`, `u64`
- Floating point: `f32`, `f64`
- `boolean`
- `char`
- `atom`
- `str`
- `void`
- `never`

### 3.2 `str`

- `str` is a primitive string-like type.

### 3.3 `atom`

`atom` is a primitive type for interned labels. Atom literals use the `:` prefix.

```ignis
function status(ok: boolean): atom {
    if (ok) {
        return :ok;
    }
    return :error;
}
```

### 3.4 `null`

`null` is a literal of type `NullPtr`. It coerces to any pointer type when the context provides one.

```ignis
let p: *i32 = null;          // coerces to *i32
let q: *mut u8 = null;       // coerces to *mut u8

if (p == null) {              // pointer comparison
    return -1;
}
```

Using `null` where a non-pointer type is expected is a compile-time error. Dereferencing `null` and arithmetic on `null` are also rejected at compile time. `null` can appear as a match pattern on pointer-typed scrutinees.

### 3.5 `never`

`never` is the bottom type. It is the type of expressions that never produce a value: `@panic(...)`, `@trap()`, `@unreachable()`. It is compatible with any other type, so a function that always panics satisfies any return type.

```ignis
function fail(): i32 {
    @panic("fatal");
    // no return needed: @panic has type never
}
```

### 3.6 References and Pointers

```ignis
function main(): i32 {
    let mut x: i32 = 41;

    let rx: &i32 = &x;
    let mx: &mut i32 = &mut x;

    let p: *i32 = &x as *i32;
    let mp: *mut i32 = &mut x as *mut i32;

    *mp = *mp + 1;
    return *rx;
}
```

### 3.7 Arrays and Function Types

```ignis
type Mapper = (i32) -> i32;

function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[0];
}
```

Note: fixed-size arrays (`T[N]`) are supported. Dynamic array type syntax (`T[]`) is parsed but currently rejected by semantic analysis.

### 3.8 Generic Type Use

```ignis
record Box<T> {
    public value: T;
}

function main(): i32 {
    let b: Box<i32> = Box { value: 42 };
    return b.value;
}
```

## 4. Declarations

### 4.1 Functions

```ignis
function add(a: i32, b: i32): i32 {
    return a + b;
}

inline(always) function hotPath(x: i32): i32 {
    return x + 1;
}
```

The `inline` modifier comes before `function`. Three variants exist:

- `inline function f() { ... }` -- hint to inline
- `inline(always) function f() { ... }` -- force inline
- `inline(never) function f() { ... }` -- prevent inline

Generic functions:

```ignis
function identity<T>(value: T): T {
    return value;
}
```

Generic type arguments can be inferred from call arguments:

```ignis
let x = identity(42);       // T inferred as i32
let y = identity<i64>(42);  // T explicitly i64
```

Function overloading is supported by signature differences.

### 4.2 Variables and Constants

Variable declarations support optional type annotations. When the type is omitted, it is inferred from the initializer or from later usage.

```ignis
let x: i32 = 10;       // explicit type
let y = 20;             // inferred as i32 from literal
let mut z = x + y;      // inferred as i32 from expression
const LIMIT: i32 = 100; // constants require a type annotation
```

Deferred inference is also supported. A variable can be declared without type or initializer and resolved when first assigned:

```ignis
let mut value;           // type unknown here
value = computeResult(); // type inferred from assignment
```

If an inference variable is never constrained, the compiler reports an error.

### 4.3 Literal Coercion

Numeric literals adapt to the expected type from context. A plain integer literal like `42` defaults to `i32`, but coerces to `i8`, `u16`, `i64`, etc. if the target type requires it and the value fits within the target range. Float literals coerce similarly between `f32` and `f64`.

```ignis
let a: u8 = 255;        // 255 fits in u8, coerced
let b: i64 = 1000000;   // coerced to i64
let c: f32 = 3.14;      // coerced to f32
```

Overflow is a compile-time error: `let x: u8 = 256;` fails.

### 4.4 Type Aliases

```ignis
type Id = i32;
type Pair<T> = (T, T);
```

### 4.5 Records

Fields and methods are private by default.

```ignis
record Counter {
    public value: i32;

    get(&self): i32 {
        return self.value;
    }

    increment(&mut self): void {
        self.value += 1;
        return;
    }

    public static new(start: i32): Counter {
        return Counter { value: start };
    }
}
```

Records support static fields and static methods:

```ignis
record Config {
    static MAX_SIZE: i32 = 1024;

    public static default(): Config {
        return Config {};
    }
}

let max = Config::MAX_SIZE;
```

### 4.6 Enums

```ignis
enum Option<T> {
    SOME(T),
    NONE,
}
```

Enums can include methods, static methods, and static fields:

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

### 4.7 Traits

```ignis
trait Describable {
    describe(&self): i32;

    code(&self): i32 {
        return 0;
    }
}

@implements(Describable)
record Item {
    public id: i32;

    describe(&self): i32 {
        return self.id;
    }
}
```

### 4.8 Namespaces

```ignis
namespace Math {
    function add(a: i32, b: i32): i32 {
        return a + b;
    }
}

function main(): i32 {
    return Math::add(1, 2);
}
```

### 4.9 Imports, Exports, Extern

```ignis
import Io from "std::io";
import println, print from "std::io";

export function run(): void {
    Io::println("run");
    return;
}

extern libc {
    function puts(s: str): i32;
    const BUFSIZ: i32;
}
```

Extern blocks support qualified paths and can contain both function signatures and constant declarations (without initializers):

```ignis
extern std::io {
    function write(fd: i32, buf: *u8, count: u64): i64;
}

extern __errno {
    const ENOENT: i32;
    const EACCES: i32;
}
```

#### Side-Effect Imports

`import _ from "..."` loads a module purely for its side effects (e.g., namespace contributions) without binding any name into the importing scope.

```ignis
import _ from "std::libc::memory";   // loads module, no binding
import _ from "./sub_module";         // relative path works too
```

`_` must be the sole item in the import statement. Combining it with named items (`import _, X from "..."`) is an error.

#### Re-Export From

`export X from "..."` imports a symbol and immediately re-exports it. Equivalent to `import X from "..."; export X;` but as a single statement.

```ignis
export CType from "./primitives";
export Foo, Bar from "./lib";
```

The re-exported symbols are available both in the current module's scope and to consumers that import from this module. `export _ from "..."` is not allowed -- use `import _ from "..."` instead.

#### Import Path Resolution

The `from` string is resolved in this order:

1. **Alias match** -- The first segment (before `::`) is checked against the project's `[aliases]` table from `ignis.toml`. Exact segment match is tried first, then prefix match for shorter alias keys. The `"std"` alias is always present and maps to the standard library. Other aliases resolve to the target directory, trying `<dir>/mod.ign` first and falling back to `<dir>.ign`.

2. **Relative path** (`./`, `../`) -- Resolved from the importing file's directory.

3. **Bare path** -- Resolved from the project root (source directory).

```toml
# ignis.toml
[aliases]
mylib = "libs/mylib"
"@" = "./src"
```

```ignis
import Utils from "mylib::utils";     // exact match: libs/mylib/utils.ign
import Sub from "mylib::sub::mod";    // exact match: libs/mylib/sub/mod.ign
import Token from "@token::token";    // prefix match: ./src/token/token.ign
import Lexer from "@lexer";           // prefix match: ./src/lexer.ign
```

The `"std"` alias is reserved and cannot be overridden in `[aliases]`.

### 4.10 Extension Methods

```ignis
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

@extension(i32, mut)
function bump(value: i32): i32 {
    return value + 1;
}

function main(): i32 {
    let mut x: i32 = 20;
    let y: i32 = x.doubled();
    return y + x.bump();
}
```

## 5. Closures

Closures are anonymous functions that can capture variables from their enclosing scope.

### 5.1 Syntax

```
(params): ReturnType -> expression
(params): ReturnType -> { block }
```

```ignis
let add = (a: i32, b: i32): i32 -> a + b;
let greet = (): void -> { Io::println("hi"); };
let double = (x: i32): i32 -> x * 2;
```

Closures can be passed as arguments, stored in variables, and used as module-level constants:

```ignis
const add: (i32, i32) -> i32 = (a: i32, b: i32): i32 -> a + b;

function apply(@noescape f: (i32) -> i32, x: i32): i32 {
    return f(x);
}

function main(): i32 {
    return apply((n: i32): i32 -> n * 2, 21);  // 42
}
```

### 5.2 Capture Modes

Closures automatically capture referenced variables from the enclosing scope. The capture mode is inferred from usage:

| Usage inside closure | Inferred mode | Effect |
|---|---|---|
| Read-only, Copy type | By value | Copy of the value at creation |
| Read-only, non-Copy type | By shared reference | `*T` pointer into enclosing scope |
| Mutated | By mutable reference | `*mut T` pointer into enclosing scope |
| Moved (non-Copy consumed) | By value | Ownership transferred at creation |

Manual overrides are available inside the closure body:

```ignis
let mut x: i32 = 10;
let get_x = (): i32 -> @move x;     // force by-value (snapshot)
x = 99;
return get_x();                       // 10, not 99

let see_x = (): i32 -> @ref x;      // force shared reference
let inc_x = (): void -> @refMut x;  // force mutable reference
```

### 5.3 Escape Analysis and `@noescape`

When a closure is passed to a function parameter marked `@noescape`, the compiler guarantees the closure does not outlive the call. This allows safe by-reference captures without heap allocation.

If a closure escapes (stored in a field, returned, passed to a non-`@noescape` parameter) while capturing variables by reference, the compiler reports an error to prevent dangling references.

```ignis
function forEach(data: *i32, len: i32, @noescape f: (i32) -> void): void {
    let mut i: i32 = 0;
    while (i < len) {
        f(data[i as u64]);
        i = i + 1;
    }
}

function main(): i32 {
    let arr: i32[3] = [10, 20, 12];
    let mut sum: i32 = 0;
    forEach((&arr[0]) as *i32, 3, (x: i32): void -> { sum = sum + x; });
    return sum;  // 42
}
```

## 6. Ownership and Borrowing

Ignis tracks ownership of values that need cleanup (types with `@implements(Drop)` or containing such types). Non-Copy types are moved by default; using a value after it has been moved, dropped, or freed is a compile-time error.

### 6.1 Move Semantics

```ignis
@implements(Drop)
record Resource {
    public id: i32;
    drop(&mut self): void { return; }
}

function main(): i32 {
    let r = Resource { id: 1 };
    let r2 = r;          // r is moved to r2
    // return r.id;      // ERROR: use after move
    return r2.id;
}
```

Passing a non-Copy value to a function also moves it:

```ignis
function consume(r: Resource): i32 {
    return r.id;
}

let r = Resource { id: 42 };
let x = consume(r);
// r is no longer valid here
```

Re-assigning to a moved variable makes it valid again:

```ignis
let mut r = Resource { id: 1 };
let r2 = r;              // r moved
r = Resource { id: 2 };  // r valid again
```

### 6.2 Copy and Structural Copy

Primitive types (`i32`, `boolean`, `char`, pointers, references, etc.) are Copy -- assignment copies the value, and the original remains valid.

Records and enums are structurally Copy if all their fields (or variant payloads) are recursively Copy. No annotation is needed:

```ignis
record Vec2 {
    public x: f32;
    public y: f32;
}

let a = Vec2 { x: 1.0, y: 2.0 };
let b = a;     // copy, both a and b are valid
```

Explicit `@implements(Copy)` is also supported and validated (all fields must be Copy). A type with `@implements(Drop)` is never Copy, even if all fields are primitive.

### 6.3 Clone

`@implements(Clone)` requires a `clone(&self): Self` method. Calling `.clone()` produces an independent copy without moving the original:

```ignis
@implements(Drop, Clone)
record Buffer {
    public len: i32;
    drop(&mut self): void { return; }
    clone(&self): Buffer { return Buffer { len: self.len }; }
}

let a = Buffer { len: 10 };
let b = a.clone();   // a is NOT moved
// both a and b are valid
```

### 6.4 Drop

Types with `@implements(Drop)` must provide a `drop(&mut self): void` method. The compiler automatically inserts drop calls at scope exits, early returns, `break`/`continue`, and before overwriting a live variable.

Double-drop and use-after-drop are compile-time errors. A runtime drop guard provides defense-in-depth.

### 6.5 Borrow Checking

References follow exclusivity rules:

- Multiple `&T` (shared references) are allowed simultaneously.
- A single `&mut T` (mutable reference) is exclusive -- no other references may coexist.
- Mutating a variable while any borrow is active is an error.
- Returning a reference to a local variable is an error.

```ignis
let mut x: i32 = 10;
let r1 = &x;
let r2 = &x;          // OK: multiple shared refs
// let m = &mut x;     // ERROR: mutable ref while shared refs exist
```

### 6.6 FFI Ownership

Extern functions do not consume ownership by default. To indicate that an extern function takes ownership of a parameter, use `@takes`:

```ignis
extern rt {
    function release(@takes ptr: *mut void): void;
}
```

Without `@takes`, passing a non-Copy owned value to an extern function produces a warning about potential memory leaks.

## 7. Attributes

Attributes use `@name` or `@name(args)` and are applied to declarations.

### Declaration attributes

- `@implements(...)` -- lang traits (`Drop`, `Clone`, `Copy`) or user-defined traits
- `@packed` -- remove struct padding
- `@aligned(N)` -- set minimum alignment
- `@cold` -- mark function as unlikely to execute
- `@externName("...")` -- override the C symbol name for a function
- `@deprecated` / `@deprecated("...")` -- mark as deprecated
- `@allow(...)`, `@warn(...)`, `@deny(...)` -- lint level overrides
- `@extension(Type)` / `@extension(Type, mut)` -- extension methods

### Parameter attributes

- `@takes` -- marks an extern function parameter as ownership-consuming
- `@noescape` -- marks a closure parameter as non-escaping (enables safe ref captures)

```ignis
extern rt {
    function release(@takes ptr: *mut void): void;
}

function forEach(@noescape f: (i32) -> void, data: *i32, len: i32): void {
    // f is guaranteed not to escape this call
}
```

## 8. Statements and Control Flow

### 8.1 `if` / `else`

Parentheses are required in conditions.

```ignis
if (x > 0) {
    return 1;
} else {
    return 0;
}
```

### 8.2 `if let`

`let PATTERN = EXPR` can be used as a condition.

```ignis
if (let Option::SOME(v) = maybeValue()) {
    return v;
}
```

With condition chains:

```ignis
if (let Option::SOME(x) = maybeX() && x > 10) {
    return x;
}
```

### 8.3 `while` and `while let`

```ignis
while (i < 10) {
    i += 1;
}

while (let Option::SOME(v) = nextValue()) {
    total += v;
}
```

### 8.4 `let else`

`let else` is a statement:

```ignis
let Option::SOME(v) = maybeValue() else {
    return -1;
};
```

The `else` block must diverge (`return`, `break`, `continue`, `@panic`, etc.).

### 8.5 `for` (C-style)

The type annotation on the loop variable is optional (inferred from the initializer).

```ignis
for (let i = 0; i < 10; i++) {
    sum += i;
}

for (let i: i32 = 0; i < 10; i++) {
    sum += i;
}
```

### 8.6 `for of`

```ignis
let arr: i32[3] = [1, 2, 3];

for (let x of arr) {
    sum += x;
}

for (let x: &i32 of arr) {
    sum += *x;
}
```

### 8.7 Other statements

```ignis
return;
return 42;

break;
continue;
```

## 9. Expressions

### 9.1 Literals

```ignis
42
3.14
true
false
'a'
"hello"
0xFF
0b1010
null
:ok
[1, 2, 3]
```

### 9.2 Atoms

An atom literal is `:` followed by an identifier.

```ignis
let state: atom = :ready;

match (state) {
    :ready -> return 1,
    :error -> return -1,
    _ -> return 0,
}
```

### 9.3 Record initialization

```ignis
let p: Point = Point { x: 1, y: 2 };
let p2 = Point { x: 3, y: 4 };     // type inferred
```

### 9.4 Calls, paths, member access, indexing

```ignis
let n: i32 = Math::add(1, 2);
let x: i32 = counter.get();
let y: i32 = values[0];
let z: i32 = identity<i32>(x);
let w = identity(x);              // type arg inferred
```

### 9.5 Operators

Arithmetic: `+ - * / %`

Comparison: `== != < > <= >=`

Logical: `&& || !`

Bitwise: `& | ^ ~ << >>`

Assignment: `= += -= *= /= %= &= |= ^= <<= >>=`

Pipe: `|>` (left-associative, see [Section 9.11](#911-pipe-operator))

Other: cast `expr as Type`, postfix `x++`, `x--`, prefix `++x`, `--x`.

### 9.6 Ternary operator

Ignis supports `cond ? thenExpr : elseExpr`.

```ignis
let x: i32 = isReady ? 1 : 0;
let y: i32 = x > 10 ? x : 10;
```

### 9.7 Cast operator

```ignis
let a: i64 = 42 as i64;
let p: *i32 = &value as *i32;
```

### 9.8 Match expressions

```ignis
let result: i32 = match (value) {
    Option::SOME(x) if x > 0 -> x,
    Option::SOME(_) -> 0,
    Option::NONE -> -1,
};
```

Arms accept expression bodies or block bodies.

### 9.9 Closure expressions

See [Section 5. Closures](#5-closures).

### 9.10 Builtin call expressions

Builtins with `@` are regular expressions and accept type args where applicable.

```ignis
let size: u64 = @sizeOf<i32>();
let ptr: *mut u8 = @pointerFromInteger<*mut u8>(addr);
let ok: boolean = @configFlag("os.linux");
```

### 9.11 Pipe Operator

The pipe operator `|>` passes a value as the first argument to a function on the right-hand side. `lhs |> rhs` desugars to a call where `lhs` is prepended to the argument list.

```ignis
function double(x: i32): i32 {
    return x * 2;
}

function add(x: i32, y: i32): i32 {
    return x + y;
}

function main(): i32 {
    return 10 |> double;           // double(10) = 20
}
```

Pipes are left-associative and can be chained:

```ignis
let result = 1 |> add(2) |> add(3);  // add(add(1, 2), 3) = 6
```

#### Supported RHS forms

| RHS form | Desugar | Example |
|---|---|---|
| Bare function | `f(lhs)` | `x \|> double` |
| Namespace/static path | `Ns::f(lhs)` | `x \|> Math::square` |
| Call with extra args | `f(lhs, a, b)` | `x \|> add(10)` |
| Generic path call | `Ns::f<T>(lhs, a)` | `x \|> identity<i32>` |
| Lambda | `((p): R -> body)(lhs)` | `x \|> (n: i32): i32 -> n * 2` |
| Bare method | `obj.m(lhs)` | `x \|> list.push` |
| Method call | `obj.m(lhs, a)` | `x \|> list.insert(0)` |

For method calls, the receiver (`obj`) is bound to `self` as usual, and the piped value is inserted into the explicit argument list (prepended or at the placeholder position). Placeholders work the same as with free function calls.

#### Precedence

`|>` binds between `||` and the ternary `?`:

```ignis
a || b |> f       // a || (b |> f)
a |> f ? x : y    // (a |> f) ? x : y
```

#### Placeholder

When the RHS is a call expression, `_` can be used as a placeholder to indicate
where the piped value is inserted:

```ignis
x |> f(1, _, 3)    // f(1, x, 3)
x |> f(_, 1)        // f(x, 1) — equivalent to x |> f(1) without placeholder
```

Without a placeholder, the piped value is prepended as the first argument.
Only one `_` per pipe step is allowed. `_` outside pipe call arguments is
an error.

#### Evaluation order

In `lhs |> f(extra1, extra2)`, `lhs` is evaluated first, then `extra1`, then `extra2`, then the call. This follows naturally from left-to-right argument evaluation.

```ignis
trace(0) |> f(trace(1)) |> g(trace(2))
// Desugars to: g(f(trace(0), trace(1)), trace(2))
// Order: trace(0), trace(1), f(...), trace(2), g(...)
```

## 10. Pattern Syntax

Patterns are used by `match`, `if let`, `while let`, and `let else`.

Supported pattern forms:

- Wildcard: `_`
- Literal: `1`, `true`, `"x"`, `:ok`, `null`
- Binding: `name`
- Variant path: `Option::SOME(x)`
- Tuple pattern: `(x, y)`
- Or pattern: `A | B`
- Guard in match arm: `pattern if condition`

Example:

```ignis
match (v) {
    Option::SOME(1 | 2) -> 1,
    Option::SOME(x) if x > 2 -> x,
    _ -> 0,
}
```

## 11. Builtins and Compiler Directives

Directive builtins use `@name(...)` syntax:

```ignis
let s: u64 = @sizeOf<i32>();
let a: u64 = @alignOf<i32>();
let t: str = @typeName<i32>();

let p: *mut u8 = @pointerFromInteger<*mut u8>(addr);
let n: u64 = @integerFromPointer(p);

@panic("fatal");
```

Common directive builtins:

- `@configFlag`
- `@compileError`
- `@sizeOf`, `@alignOf`, `@typeName`
- `@bitCast`, `@pointerCast`, `@integerFromPointer`, `@pointerFromInteger`
- `@read`, `@write`
- `@dropInPlace`, `@dropGlue`
- `@panic`, `@trap`, `@unreachable`
- `@maxOf`, `@minOf`

## 12. Semantics Summary

- `if` and `while` conditions must be parenthesized.
- `let` conditions are valid in conditional contexts and chain naturally with `&&`.
- `let` conditions in `||` expressions are rejected.
- `let else` requires an `else` branch that always diverges.
- `let` declarations support type inference: `let x = expr;` infers the type from the initializer. Deferred inference (`let mut x;` resolved on first assignment) is also supported.
- Numeric literals coerce to the expected type if the value fits.
- Extension methods can be defined on supported target types with `@extension(...)`.
- Calling mutating methods (or mut extensions) requires a mutable receiver.
- Non-Copy types are moved on assignment and function call; use-after-move is a compile-time error.
- `@implements(Drop)` types are automatically dropped at scope exit; double-drop is a compile-time error.
- The pipe operator `|>` desugars to a function call with the LHS as the first argument. It supports bare functions, namespace paths, calls with extra args, generic calls, lambdas, and instance method calls. The `_` placeholder controls argument insertion position.
- User-defined trait checks currently target records.
- `T[]` is parsed but rejected semantically (dynamic vectors are not enabled).
- Use `str` for primitive string slices; there is no `string` type keyword in current syntax.

## 13. Minimal Complete Example

```ignis
import Io from "std::io";

enum Maybe {
    SOME(i32),
    NONE,
}

function maybePositive(value: i32): Maybe {
    if (value > 0) {
        return Maybe::SOME(value);
    }
    return Maybe::NONE;
}

function double(x: i32): i32 {
    return x * 2;
}

function main(): i32 {
    let value = maybePositive(42);

    if (let Maybe::SOME(v) = value && v > 0) {
        let result = v |> double |> (n: i32): i32 -> n + 1;
        Io::println("ok");
        return result;
    }

    return 0;
}
```
