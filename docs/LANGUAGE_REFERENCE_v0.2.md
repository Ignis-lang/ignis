# Ignis Language Reference v0.2

This document describes the Ignis programming language as implemented in version 0.2.x.

## Changes from v0.2.0 to v0.2.1

### New Features

- **Visibility control**: Fields and methods are private by default; use `public` keyword
- **Mutable pointers**: Explicit `*mut T` syntax for mutable pointers
- **Mutable self**: Methods can use `&mut self` for mutating receivers
- **Doc comments**: `///` and `/** */` for documentation

## Changes from v0.1

### New Features

- **Generics**: Type parameters for functions, records, enums, and type aliases
- **Records**: User-defined struct types with fields and methods
- **Enums**: Algebraic data types with variants
- **Namespaces**: Module-level organization and scoping
- **Extern blocks**: FFI declarations grouped by library
- **Type aliases**: Named type abbreviations with optional type parameters
- **For-of loops**: Iteration over arrays and vectors
- **Ternary expressions**: `condition ? then : else`
- **Function overloading**: Multiple functions with the same name but different signatures
- **Null pointer type**: Explicit null pointer handling

### Syntax Changes

- Import syntax uses namespaced modules: `import Io from "std::io"`
- Namespace access uses `::` operator: `Math::add(1, 2)`

## Types

### Primitive Types

| Type | Description | Size |
|------|-------------|------|
| `i8` | Signed 8-bit integer | 1 byte |
| `i16` | Signed 16-bit integer | 2 bytes |
| `i32` | Signed 32-bit integer | 4 bytes |
| `i64` | Signed 64-bit integer | 8 bytes |
| `u8` | Unsigned 8-bit integer | 1 byte |
| `u16` | Unsigned 16-bit integer | 2 bytes |
| `u32` | Unsigned 32-bit integer | 4 bytes |
| `u64` | Unsigned 64-bit integer | 8 bytes |
| `f32` | 32-bit floating point | 4 bytes |
| `f64` | 64-bit floating point | 8 bytes |
| `boolean` | Boolean value (`true`/`false`) | 1 byte |
| `char` | Unicode code point | 4 bytes |
| `string` | String (null-terminated) | pointer |
| `void` | No value | 0 bytes |

### Records

User-defined types with fields and optional methods:

```ignis
record Point {
    x: i32;
    y: i32;
}

record Person {
    public name: string;
    public age: i32;
}
```

#### Visibility

Fields and methods are **private by default**. Use the `public` keyword to make them accessible from outside the record:

```ignis
record Account {
    public balance: i32;    // Accessible from anywhere
    secret: string;         // Private - only accessible within Account methods
    
    public getBalance(&self): i32 {
        return self.balance;
    }
    
    private validate(&self): boolean {  // Explicitly private
        return self.secret != "";
    }
}

function main(): void {
    let acc: Account = Account { balance: 100, secret: "key" };
    let b: i32 = acc.balance;     // OK - balance is public
    // let s: string = acc.secret; // ERROR - secret is private
    return;
}
```

Private members can always be accessed within the record's own methods via `self`.

#### Record Instantiation

```ignis
let p: Point = Point { x: 10, y: 20 };
let person: Person = Person { name: "Alice", age: 30 };
```

#### Field Access

```ignis
let x: i32 = p.x;
let name: string = person.name;
```

#### Generic Records

```ignis
record Box<T> {
    value: T;
}

let intBox: Box<i32> = Box { value: 42 };
let strBox: Box<string> = Box { value: "hello" };
```

#### Multi-parameter Generics

```ignis
record Pair<A, B> {
    first: A;
    second: B;
}

let p: Pair<string, i32> = Pair { first: "age", second: 25 };
```

#### Methods

Records can have instance methods and static methods:

```ignis
record Counter {
    value: i32;
    
    /// Instance method - takes self by immutable reference
    public get(&self): i32 {
        return self.value;
    }
    
    /// Mutating method - takes self by mutable reference
    public increment(&mut self): void {
        self.value = self.value + 1;
    }
    
    /// Static method - no self parameter
    public static new(initial: i32): Counter {
        return Counter { value: initial };
    }
}

function main(): i32 {
    let mut c: Counter = Counter::new(0);
    c.increment();
    c.increment();
    return c.get();  // Returns 2
}
```

Method self parameter types:

| Self Type | Description |
|-----------|-------------|
| `&self` | Immutable reference to the receiver |
| `&mut self` | Mutable reference to the receiver |
| (none) | Static method, called via `Type::method()` |

### Enums

Algebraic data types with variants:

```ignis
enum Option<T> {
    SOME(T),
    NONE,
}

enum Result<T, E> {
    OK(T),
    ERROR(E),
}
```

### Arrays/Vectors

Fixed-size arrays:

```ignis
let arr: i32[5] = [1, 2, 3, 4, 5];
let first: i32 = arr[0];
```

### References

```ignis
let x: i32 = 42;
let r: &i32 = &x;           // Immutable reference
let mut y: i32 = 10;
let mr: &mut i32 = &mut y;  // Mutable reference
```

### Pointers

Ignis supports both immutable and mutable pointers:

```ignis
let x: i32 = 42;
let p: *i32 = &x as *i32;     // Immutable pointer
let value: i32 = *p;           // Dereference

let mut y: i32 = 10;
let mp: *mut i32 = &mut y as *mut i32;  // Mutable pointer
*mp = 20;                               // Write through mutable pointer
```

#### Pointer Types

| Type | Description |
|------|-------------|
| `*T` | Immutable pointer to T |
| `*mut T` | Mutable pointer to T |

Mutable pointers are required to modify the pointed-to value. Attempting to write through an immutable pointer is a compile-time error.

### Type Aliases

Simple type aliases give alternative names to existing types:

```ignis
type Integer = i32;
type StringList = string[];
```

#### Generic Type Aliases

Type aliases can have type parameters:

```ignis
type Identity<T> = T;
type BoxAlias<T> = Box<T>;
type First<A, B> = A;

// Usage
let x: Identity<i32> = 42;
let box: BoxAlias<boolean> = Box { value: true };
let f: First<i32, string> = 10;
```

When a generic type alias is instantiated, the type arguments are substituted into the target type:

```ignis
type IntBox = Box<i32>;  // Non-generic alias to a generic type

let b1: IntBox = Box { value: 100 };
let b2: BoxAlias<i32> = Box { value: 200 };  // Equivalent to IntBox
```

## Declarations

### Functions

```ignis
function name(param1: Type1, param2: Type2): ReturnType {
    return value;
}
```

#### Generic Functions

```ignis
function identity<T>(value: T): T {
    return value;
}

let x: i32 = identity<i32>(42);
```

### Function Overloading

Multiple functions with the same name but different parameter types:

```ignis
function greet(name: string): string {
    return name;
}

function greet(age: i32): i32 {
    return age;
}

function main(): void {
    let s: string = greet("Alice");  // Calls first overload
    let n: i32 = greet(42);          // Calls second overload
    return;
}
```

### Variables

```ignis
let x: i32 = 42;              // Immutable
let mut y: i32 = 10;          // Mutable
y = 20;                       // OK
```

### Constants

```ignis
const MAX_SIZE: i32 = 100;
const PI: f64 = 3.14159265359;
```

### Namespaces

Group related declarations:

```ignis
namespace Math {
    function add(a: i32, b: i32): i32 {
        return a + b;
    }

    function multiply(a: i32, b: i32): i32 {
        return a * b;
    }
}

function main(): void {
    let sum: i32 = Math::add(1, 2);
    let product: i32 = Math::multiply(3, 4);
    return;
}
```

#### Nested Namespaces

```ignis
namespace Outer {
    namespace Inner {
        function foo(): i32 {
            return 42;
        }
    }
}

let x: i32 = Outer::Inner::foo();
```

### Extern Blocks

Group FFI declarations by library:

```ignis
extern libc {
    function puts(s: string): i32;
    function printf(format: string): i32;
}

function main(): void {
    libc::puts("Hello, World!");
    return;
}
```

## Comments

### Line Comments

```ignis
// This is a single-line comment
let x: i32 = 42; // End-of-line comment
```

### Block Comments

```ignis
/* This is a
   multi-line block comment */
let y: i32 = 10;
```

### Documentation Comments

Documentation comments are attached to the following declaration and appear in IDE hover information:

```ignis
/// Calculates the sum of two integers.
/// 
/// # Arguments
/// * `a` - The first operand
/// * `b` - The second operand
///
/// # Returns
/// The sum of a and b.
function add(a: i32, b: i32): i32 {
    return a + b;
}

/**
 * A 2D point in Cartesian coordinates.
 * 
 * Use Point::origin() to create a point at (0, 0).
 */
record Point {
    public x: i32;
    public y: i32;
    
    /// Creates a point at the origin.
    public static origin(): Point {
        return Point { x: 0, y: 0 };
    }
}
```

Doc comment syntax:
- `///` - Single-line doc comment
- `/** ... */` - Multi-line doc comment

## Expressions

### Literals

```ignis
42              // i32
3.14            // f64
true            // boolean
false           // boolean
'a'             // char
"hello"         // string
0xFF            // hex integer
0b1010          // binary integer
null            // null pointer
```

### Operators

#### Arithmetic

| Operator | Description |
|----------|-------------|
| `+` | Addition |
| `-` | Subtraction |
| `*` | Multiplication |
| `/` | Division |
| `%` | Modulo |

#### Comparison

| Operator | Description |
|----------|-------------|
| `==` | Equal |
| `!=` | Not equal |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less or equal |
| `>=` | Greater or equal |

#### Logical

| Operator | Description |
|----------|-------------|
| `&&` | Logical AND (short-circuit) |
| `\|\|` | Logical OR (short-circuit) |
| `!` | Logical NOT |

#### Bitwise

| Operator | Description |
|----------|-------------|
| `&` | Bitwise AND |
| `\|` | Bitwise OR |
| `^` | Bitwise XOR |
| `~` | Bitwise NOT |
| `<<` | Left shift |
| `>>` | Right shift |

#### Ternary

```ignis
let max: i32 = a > b ? a : b;
```

#### Increment/Decrement

```ignis
let mut x: i32 = 5;
x++;  // x is now 6
x--;  // x is now 5
```

#### Compound Assignment

```ignis
let mut x: i32 = 10;
x += 5;   // x = x + 5
x *= 2;   // x = x * 2
```

### Type Casts

```ignis
let x: i32 = 42;
let y: f64 = x as f64;
```

## Statements

### If/Else

```ignis
if (x > 0) {
    // then branch
} else {
    // else branch
}
```

Chained conditions:

```ignis
if (x > 10) {
    // ...
} else if (x > 5) {
    // ...
} else {
    // ...
}
```

### While Loop

```ignis
while (condition) {
    // body
}
```

Example:

```ignis
let mut i: i32 = 0;
while (i < 10) {
    i = i + 1;
}
```

### For Loop (C-style)

```ignis
for (let i = 0; i < 10; i++) {
    // body
}
```

With explicit type:

```ignis
for (let i: i64 = 0; i < 10; i++) {
    // body
}
```

### For-Of Loop

Iterate over arrays and vectors:

```ignis
let arr: i32[3] = [1, 2, 3];

// By value (for Copy types)
for (let x of arr) {
    // x is i32
}

// By reference
for (let x: &i32 of arr) {
    // x is &i32
}
```

### Return

```ignis
function foo(): i32 {
    return 42;
}

function bar(): void {
    return;
}
```

### Break and Continue

```ignis
while (true) {
    if (done) {
        break;
    }
    if (skip) {
        continue;
    }
}
```

## Modules

### Importing

```ignis
import println from "std::io";
import sqrt, PI from "std::math";
import MyFunc as Alias from "./local_module";
```

### Importing Namespaces

```ignis
import Io from "std::io";
import String from "std::string";

Io::println("Hello");
let s: string = String::concat("a", "b");
```

### Exporting

```ignis
export function publicFunction(): void {
    return;
}

export const PUBLIC_CONSTANT: i32 = 42;

export record PublicRecord {
    value: i32;
}
```

## Standard Library

### std::io

```ignis
import Io from "std::io";

Io::println("Hello, World!");
Io::print("No newline");
```

### std::string

```ignis
import String from "std::string";

let len: u64 = String::length("hello");
let combined: string = String::concat("Hello, ", "World!");
let numStr: string = String::toString(42);
```

### std::memory

```ignis
import allocate, deallocate from "std::memory";

let p: *mut u8 = allocate(16);
// ... use memory ...
deallocate(p);
```

## Known Limitations

### Not Yet Implemented

- Pattern matching (`match` expressions)
- Closures / Lambdas
- Traits / Interfaces
- Enum methods
- Associated types

### Runtime Limitations

- No runtime bounds checking for arrays
- No garbage collection (manual memory management)
- Strings are null-terminated C strings

## Entry Point

```ignis
function main(): void {
    return;
}

// Or with exit code:
function main(): i32 {
    return 0;
}
```
