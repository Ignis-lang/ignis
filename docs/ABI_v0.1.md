# Ignis ABI Reference v0.1

This document describes the Application Binary Interface (ABI) for Ignis v0.1.

## Overview

Ignis compiles to C code, which is then compiled with GCC to produce native executables. This document describes how Ignis types and constructs map to C.

## Type Mapping

Ignis types map to C types as defined in `std/runtime/types/types.h`:

| Ignis Type | C Type | C Header Type |
|------------|--------|---------------|
| `i8` | `i8` | `int8_t` |
| `i16` | `i16` | `int16_t` |
| `i32` | `i32` | `int32_t` |
| `i64` | `i64` | `int64_t` |
| `u8` | `u8` | `uint8_t` |
| `u16` | `u16` | `uint16_t` |
| `u32` | `u32` | `uint32_t` |
| `u64` | `u64` | `uint64_t` |
| `f32` | `f32` | `float` |
| `f64` | `f64` | `double` |
| `boolean` | `boolean` | `uint8_t` |
| `char` | `i8` | `int8_t` |
| `string` | `string` | `char*` |
| `void` | `void` | `void` |

### Boolean Values

```c
#define TRUE 1
#define FALSE 0
```

### Null

```c
typedef void* null;
```

## Pointer and Reference Types

| Ignis Type | C Type |
|------------|--------|
| `*T` | `T*` |
| `&T` | `T*` |
| `&mut T` | `T*` |

References and pointers both compile to C pointers. The distinction between `&T` and `&mut T` is enforced at compile time by the borrow checker, not at runtime.

## Vector/Array Types

| Ignis Type | C Type |
|------------|--------|
| `T[N]` | `T[N]` (stack allocated) |

Vector literals are initialized inline:

```ignis
let arr: i32[3] = [1, 2, 3];
```

Compiles to:

```c
i32 arr[3] = {1, 2, 3};
```

## Calling Convention

Ignis uses the standard C calling convention (cdecl on x86, System V AMD64 ABI on x86-64, etc.). This is determined by GCC for the target platform.

### Function Signatures

```ignis
function add(a: i32, b: i32): i32 {
    return a + b;
}
```

Compiles to:

```c
i32 add(i32 a, i32 b) {
    return a + b;
}
```

### Void Functions

Functions with no parameters use `void` in the parameter list for C99 compliance:

```ignis
function foo(): void { return; }
```

Compiles to:

```c
void foo(void) { return; }
```

### Variadic Functions

Extern variadic functions are supported:

```ignis
extern function printf(format: string, ...): i32;
```

Compiles to:

```c
extern i32 printf(string format, ...);
```

## Entry Point

The `main` function is special-cased to always generate `int main(void)` for C compatibility:

```ignis
function main(): void {
    return;
}
```

Compiles to:

```c
int main(void) {
    return 0;
}
```

If main returns `i32`, the return value is used as the exit code.

## String Representation

Strings in Ignis v0.1 are null-terminated C strings (`char*`).

```ignis
let s: string = "Hello";
```

Compiles to:

```c
string s = "Hello";
```

### Important Notes

- Strings are NOT length-prefixed
- No UTF-8 validation is performed
- String literals are stored in read-only data sections
- Functions like `stringConcat` allocate with `malloc`; caller must `free`

This representation may change in future versions.

## The `unknown` Type

The `unknown` type (used internally for FFI and type inference) maps to `void*`:

```c
/* unknown */ void*
```

When casting from `unknown` to a concrete type, the codegen uses pointer dereferencing to handle cases where C cannot directly cast `void*` (e.g., to `float`):

```c
// Cast from unknown to f64
t1 = *(f64*)(source);
```

## Runtime Type Information

Type IDs are defined for potential future RTTI support (currently unused):

```c
#define TYPE_I8_ID      0
#define TYPE_I16_ID     1
#define TYPE_I32_ID     2
#define TYPE_I64_ID     3
#define TYPE_U8_ID      4
#define TYPE_U16_ID     5
#define TYPE_U32_ID     6
#define TYPE_U64_ID     7
#define TYPE_F32_ID     8
#define TYPE_F64_ID     9
#define TYPE_BOOL_ID    10
#define TYPE_STRING_ID  11
#define TYPE_VOID_ID    12
#define TYPE_UNKNOWN_ID 13
```

## Generated Code Structure

A typical generated C file has this structure:

```c
#include "runtime/types/types.h"
#include "ignis_std.h"  // if using std

// Forward declarations
i32 add(i32 a, i32 b);
int main(void);

// Function definitions
i32 add(i32 a, i32 b) {
    i32 t0;
    t0 = a + b;
    return t0;
}

int main(void) {
    i32 t0;
    t0 = add(1, 2);
    return 0;
}
```

## Linking

### Standard Library

The Ignis standard library compiles to:

- `libignis_std.a` - Static archive of all std modules
- `ignis_std.h` - Combined header with all declarations

### Runtime Objects

Individual runtime modules compile to object files:

- `runtime/io/libignis_io.o`
- `runtime/string/libignis_string.o`
- etc.

### Link Order

When linking, the order is:

1. User object files
2. `libignis_std.a`
3. Runtime objects
4. System libraries (`-lm` for math, etc.)

## Stability Guarantees

For v0.1, the following are considered stable:

- Type mappings in `types.h`
- Calling convention (C ABI)
- Entry point signature (`int main(void)`)

The following may change in future versions:

- String representation
- RTTI type IDs
- Internal codegen details (temporary naming, etc.)
