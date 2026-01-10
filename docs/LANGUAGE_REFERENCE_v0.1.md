# Ignis Language Reference v0.1

This document describes the Ignis programming language as implemented in version 0.1.

## Scope

### Supported Features

- **Primitive types**: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f32`, `f64`, `boolean`, `char`, `string`, `void`
- **Declarations**: `function`, `let`, `let mut`, `const`, `extern`
- **Control flow**: `if`/`else`, `while`, `for` (C-style), `return`, `break`, `continue`
- **Operators**: arithmetic, comparison, logical, bitwise (partial), increment/decrement
- **References**: `&T`, `&mut T`
- **Pointers**: `*T`
- **Type casts**: `expr as type`
- **Vectors/Arrays**: `T[N]`, literals `[a, b, c]`, access `arr[i]`
- **Modules**: `import`, `export`
- **FFI**: `extern function`, `extern const`

### NOT Supported in v0.1

The following features are planned for future versions and are not available in v0.1:

- Generics / Type parameters
- Structs / Records
- Enums
- Pattern matching (`match`)
- Closures / Lambdas
- Traits / Interfaces

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
| `char` | Single character | 1 byte |
| `string` | String (null-terminated) | pointer |
| `void` | No value | 0 bytes |

### Composite Types

#### Vectors/Arrays

Fixed-size arrays of a single element type:

```ignis
let arr: i32[5] = [1, 2, 3, 4, 5];
let first: i32 = arr[0];
```

#### References

Borrowed references to values:

```ignis
let x: i32 = 42;
let r: &i32 = &x;           // Immutable reference
let mut y: i32 = 10;
let mr: &mut i32 = &mut y;  // Mutable reference
```

#### Pointers

Raw pointers (unsafe):

```ignis
let x: i32 = 42;
let p: *i32 = &x as *i32;   // Pointer to i32
let value: i32 = *p;        // Dereference
```

## Declarations

### Functions

```ignis
function name(param1: Type1, param2: Type2): ReturnType {
    // body
    return value;
}
```

Functions must declare parameter types and return type. Use `void` for functions that don't return a value.

```ignis
function add(a: i32, b: i32): i32 {
    return a + b;
}

function greet(): void {
    println("Hello!");
    return;
}
```

### Variables

#### Immutable (default)

```ignis
let x: i32 = 42;
// x = 43;  // ERROR: cannot assign to immutable variable
```

#### Mutable

```ignis
let mut x: i32 = 42;
x = 43;  // OK
```

### Constants

Compile-time constants:

```ignis
const MAX_SIZE: i32 = 100;
const PI: f64 = 3.14159265359;
```

### Extern Declarations

For FFI with C:

```ignis
extern function puts(s: string): i32;
extern const errno: i32;
```

## Expressions

### Literals

```ignis
42          // i32
3.14        // f64
true        // boolean
false       // boolean
'a'         // char
"hello"     // string
0xFF        // hex integer
0b1010      // binary integer
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
| `&&` | Logical AND |
| `\|\|` | Logical OR |
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

#### Increment/Decrement

```ignis
let mut x: i32 = 5;
x++;  // x is now 6
x--;  // x is now 5
```

#### Compound Assignment

| Operator | Equivalent |
|----------|------------|
| `+=` | `x = x + y` |
| `-=` | `x = x - y` |
| `*=` | `x = x * y` |
| `/=` | `x = x / y` |
| `%=` | `x = x % y` |
| `&=` | `x = x & y` |
| `\|=` | `x = x \| y` |
| `^=` | `x = x ^ y` |
| `<<=` | `x = x << y` |
| `>>=` | `x = x >> y` |

```ignis
let mut x: i32 = 10;
x += 5;   // x is now 15
x *= 2;   // x is now 30
```

### Type Casts

```ignis
let x: i32 = 42;
let y: f64 = x as f64;

let a: i64 = 1000;
let b: i32 = a as i32;  // Warning: possible precision loss
```

### Function Calls

```ignis
let result: i32 = add(1, 2);
println("Hello");
```

### References and Dereferences

```ignis
let x: i32 = 42;
let r: &i32 = &x;       // Reference
let value: i32 = *r;    // Dereference

let mut y: i32 = 10;
let mr: &mut i32 = &mut y;
*mr = 20;               // Modify through mutable reference
```

### Vector Access

```ignis
let arr: i32[3] = [10, 20, 30];
let second: i32 = arr[1];  // 20
```

## Statements

### If/Else

```ignis
if condition {
    // then branch
} else {
    // else branch
}
```

The else branch is optional:

```ignis
if x > 0 {
    println("positive");
}
```

### While Loop

```ignis
while condition {
    // body
}
```

Example:

```ignis
let mut i: i32 = 0;
while i < 10 {
    println("loop");
    i = i + 1;
}
```

### For Loop

C-style for loop:

```ignis
for (let i = 0; i < 10; i++) {
    // body
}
```

All three parts (init, condition, update) are required.

The initializer supports both type inference and explicit type annotation:

```ignis
for (let i = 0; i < 10; i++) { }          // Type inferred as i32
for (let i: i64 = 0; i < 10; i++) { }     // Explicit type annotation
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
while true {
    if done {
        break;     // Exit loop
    }
    if skip {
        continue;  // Skip to next iteration
    }
}
```

## Modules

### Importing

```ignis
import { println, print } from "io";
import { sqrt, PI } from "math";
```

### Exporting

```ignis
export function publicFunction(): void {
    return;
}

export const PUBLIC_CONSTANT: i32 = 42;
```

## Standard Library

### io

```ignis
import { print, println, eprint, eprintln } from "io";

println("Hello, World!");     // Print with newline
print("No newline");          // Print without newline
eprintln("Error message");    // Print to stderr
```

### string

```ignis
import { stringLength, stringConcat, stringCompare } from "string";

let len: u64 = stringLength("hello");
let combined: string = stringConcat("Hello, ", "World!");
let cmp: i32 = stringCompare("a", "b");
```

### math

```ignis
import { sin, cos, sqrt, pow, PI, E } from "math";

let x: f64 = sin(PI / 2.0);
let y: f64 = sqrt(2.0);
let z: f64 = pow(2.0, 10.0);
```

## Borrow Checking

Ignis implements basic borrow checking similar to Rust:

1. You can have multiple immutable references OR one mutable reference (not both)
2. References must not outlive the referenced value
3. Cannot mutate a value while it's borrowed

```ignis
let mut x: i32 = 42;
let r1: &i32 = &x;
let r2: &i32 = &x;      // OK: multiple immutable refs

let mut y: i32 = 10;
let mr: &mut i32 = &mut y;
// let r: &i32 = &y;    // ERROR: cannot borrow while mutably borrowed
```

## Known Limitations

### Limited Bounds Checking

The compiler performs compile-time bounds checking for constant indices. If the index is a compile-time constant and falls outside the array bounds, a compile error is produced:

```ignis
let arr: i32[3] = [1, 2, 3];
let x: i32 = arr[5];   // COMPILE ERROR: Index 5 is out of bounds for array of size 3
let y: i32 = arr[-1];  // COMPILE ERROR: Index -1 is out of bounds for array of size 3
```

However, runtime bounds checking is NOT performed. If the index is computed at runtime and falls outside the array bounds, the behavior is undefined:

```ignis
let arr: i32[3] = [1, 2, 3];
let i: i32 = get_index();  // Returns 10 at runtime
let x: i32 = arr[i];       // UNDEFINED BEHAVIOR - no error at runtime
```

### String Representation

Strings are represented as null-terminated `char*` pointers. There is no length tracking or UTF-8 validation.

### No Garbage Collection

Memory management is manual. The standard library functions that allocate (like `stringConcat`) allocate with `malloc` and the caller is responsible for freeing.

## Entry Point

Every executable program must have a `main` function:

```ignis
function main(): void {
    // program code
    return;
}
```

The return type can be `void` or `i32`. If `i32`, the value is used as the exit code.
