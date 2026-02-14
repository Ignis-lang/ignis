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

### 3.4 References and Pointers

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

### 3.5 Arrays and Function Types

```ignis
type Mapper = (i32) -> i32;

function main(): i32 {
    let arr: i32[3] = [1, 2, 3];
    return arr[0];
}
```

Note: fixed-size arrays (`T[N]`) are supported. Dynamic array type syntax (`T[]`) is parsed but currently rejected by semantic analysis.

### 3.6 Generic Type Use

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

Generic functions:

```ignis
function identity<T>(value: T): T {
    return value;
}
```

Function overloading is supported by signature differences.

### 4.2 Variables and Constants

Variable declarations are typed:

```ignis
let x: i32 = 10;
let mut y: i32 = 20;
const LIMIT: i32 = 100;
```

Regular `let` declarations require an explicit type annotation.

### 4.3 Type Aliases

```ignis
type Id = i32;
type Pair<T> = (T, T);
```

### 4.4 Records

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

### 4.5 Enums

```ignis
enum Option<T> {
    SOME(T),
    NONE,
}
```

Enums can also include methods and fields.

### 4.6 Traits

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

### 4.7 Namespaces

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

### 4.8 Imports, Exports, Extern

```ignis
import Io from "std::io";
import println, print from "std::io";

export function run(): void {
    Io::println("run");
    return;
}

extern libc {
    function puts(s: str): i32;
}
```

### 4.9 Extension Methods

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

### 4.10 Lang Traits (`Drop`, `Clone`, `Copy`)

```ignis
@implements(Copy)
record Point {
    public x: i32;
    public y: i32;
}

@implements(Drop)
record Resource {
    public id: i32;

    drop(&mut self): void {
        return;
    }
}
```

## 5. Attributes

Attributes use `@name` or `@name(args)` and are applied to declarations.

Common examples:

- `@implements(...)`
- `@packed`
- `@aligned(N)`
- `@cold`
- `@externName("...")`
- `@deprecated` / `@deprecated("...")`
- `@allow(...)`, `@warn(...)`, `@deny(...)`
- `@extension(Type)` / `@extension(Type, mut)`

Parameter attributes are also supported. Example:

```ignis
extern rt {
    function release(@takes ptr: *mut void): void;
}
```

## 6. Statements and Control Flow

### 6.1 `if` / `else`

Parentheses are required in conditions.

```ignis
if (x > 0) {
    return 1;
} else {
    return 0;
}
```

### 6.2 `if let`

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

### 6.3 `while` and `while let`

```ignis
while (i < 10) {
    i += 1;
}

while (let Option::SOME(v) = nextValue()) {
    total += v;
}
```

### 6.4 `let else`

`let else` is a statement:

```ignis
let Option::SOME(v) = maybeValue() else {
    return -1;
};
```

The `else` block must diverge (`return`, `break`, `continue`, `panic`, etc.).

### 6.5 `for` (C-style)

```ignis
for (let i = 0; i < 10; i++) {
    sum += i;
}
```

### 6.6 `for of`

```ignis
let arr: i32[3] = [1, 2, 3];

for (let x of arr) {
    sum += x;
}

for (let x: &i32 of arr) {
    sum += *x;
}
```

### 6.7 Other statements

```ignis
return;
return 42;

break;
continue;
```

## 7. Expressions

### 7.1 Literals

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

### 7.2 Atoms

An atom literal is `:` followed by an identifier.

```ignis
let state: atom = :ready;

match (state) {
    :ready -> return 1,
    :error -> return -1,
    _ -> return 0,
}
```

### 7.3 Record initialization

```ignis
let p: Point = Point { x: 1, y: 2 };
```

### 7.4 Calls, paths, member access, indexing

```ignis
let n: i32 = Math::add(1, 2);
let x: i32 = counter.get();
let y: i32 = values[0];
let z: i32 = identity<i32>(x);
```

### 7.5 Operators

Arithmetic: `+ - * / %`

Comparison: `== != < > <= >=`

Logical: `&& || !`

Bitwise: `& | ^ ~ << >>`

Assignment: `= += -= *= /= %= &= |= ^= <<= >>=`

Other: cast `expr as Type`, postfix `x++`, `x--`, prefix `++x`, `--x`.

### 7.6 Ternary operator

Ignis supports `cond ? thenExpr : elseExpr`.

```ignis
let x: i32 = isReady ? 1 : 0;
let y: i32 = x > 10 ? x : 10;
```

### 7.7 Cast operator

```ignis
let a: i64 = 42 as i64;
let p: *i32 = &value as *i32;
```

### 7.8 Match expressions

```ignis
let result: i32 = match (value) {
    Option::SOME(x) if x > 0 -> x,
    Option::SOME(_) -> 0,
    Option::NONE -> -1,
};
```

Arms accept expression bodies or block bodies.

### 7.9 Builtin call expressions

Builtins with `@` are regular expressions and accept type args where applicable.

```ignis
let size: u64 = @sizeOf<i32>();
let ptr: *mut u8 = @pointerFromInteger<*mut u8>(addr);
let ok: boolean = @configFlag("os.linux");
```

## 8. Pattern Syntax

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

## 9. Builtins and Compiler Directives

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

Function-style builtins are also supported:

- `typeOf(expr)`
- `maxOf<T>()`
- `minOf<T>()`

## 10. Semantics That Affect Syntax

- `if` and `while` conditions must be parenthesized.
- `let` conditions are valid in conditional contexts and chain naturally with `&&`.
- `let` conditions in `||` expressions are rejected.
- `let else` requires an `else` branch that always diverges.
- Extension methods can be defined on supported target types with `@extension(...)`.
- Calling mutating methods (or mut extensions) requires a mutable receiver.
- User-defined trait checks currently target records.
- `let` declarations are typed (`let x: T = ...;`); untyped local `let` is not part of the current syntax.
- `T[]` is parsed but rejected semantically (dynamic vectors are not enabled).
- Use `str` for primitive string slices; there is no `string` type keyword in current syntax.

## 11. Minimal Complete Example

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

function main(): i32 {
    let value: Maybe = maybePositive(42);

    if (let Maybe::SOME(v) = value && v > 0) {
        Io::println("ok");
        return v;
    }

    return 0;
}
```
