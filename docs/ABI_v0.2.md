# Ignis ABI Reference v0.2

This document describes the Application Binary Interface (ABI) for Ignis v0.2.

## Changes from v0.1

- **Records**: User-defined struct types with field and method support
- **Enums**: Tagged unions with variant payloads
- **Generics**: Monomorphization with mangled names
- **Namespaces**: Underscore-separated name mangling
- **Type aliases**: Transparent (no runtime representation)

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
| `char` | `u32` | `uint32_t` |
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

## Records

Records compile to C structs. Fields are named `field_0`, `field_1`, etc.

```ignis
record Point {
    x: i32;
    y: i32;
}
```

Compiles to:

```c
struct Point {
    i32 field_0;
    i32 field_1;
};
```

### Generic Records

Generic records are monomorphized. Each instantiation produces a separate struct with a mangled name using `____` as the type separator:

```ignis
record Box<T> {
    value: T;
}

let b: Box<i32> = Box { value: 42 };
```

Compiles to:

```c
struct Box____i32 {
    i32 field_0;
};

struct Box____i32 l0;
```

### Record Methods

Instance methods receive `self` as the first parameter. Static methods are mangled with the record name:

```ignis
record Counter {
    value: i32;
    
    increment(): void {
        self.value = self.value + 1;
        return;
    }
    
    static new(): Counter {
        return Counter { value: 0 };
    }
}
```

Compiles to:

```c
void Counter_increment(struct Counter* self);
struct Counter Counter_new(void);
```

## Enums

Enums compile to tagged unions. The tag field stores the variant discriminant.

```ignis
enum Option<T> {
    SOME(T),
    NONE,
}
```

Compiles to:

```c
struct Option____i32 {
    u8 tag;
    union {
        struct {
            i32 field_0;
        } variant_0;
    } payload;
};

#define Option____i32_SOME 0
#define Option____i32_NONE 1
```

### Tag Type

The tag type is the smallest unsigned integer that can hold all variant values:

| Variants | Tag Type |
|----------|----------|
| ≤ 256 | `u8` |
| ≤ 65536 | `u16` |
| ≤ 2^32 | `u32` |

### Unit Variants

Variants without payloads do not contribute to the union:

```ignis
enum Color {
    RED,
    GREEN,
    BLUE,
}
```

Compiles to:

```c
struct Color {
    u8 tag;
};

#define Color_RED 0
#define Color_GREEN 1
#define Color_BLUE 2
```

## Namespaces

Namespace members are mangled with underscore separators:

```ignis
namespace Math {
    function add(a: i32, b: i32): i32 {
        return a + b;
    }
}
```

Compiles to:

```c
i32 Math_add(i32 a, i32 b);
```

Nested namespaces chain the underscores:

```ignis
namespace Outer {
    namespace Inner {
        function foo(): i32 { return 42; }
    }
}
```

Compiles to:

```c
i32 Outer_Inner_foo(void);
```

## Generics and Monomorphization

Generic functions and types are monomorphized: each unique instantiation generates separate code.

### Name Mangling

Generic instantiations use `____` (four underscores) as the separator between the base name and type arguments:

| Ignis | C Mangled Name |
|-------|----------------|
| `identity<i32>` | `identity____i32` |
| `identity<string>` | `identity____string` |
| `Box<i32>` | `Box____i32` |
| `Pair<i32, boolean>` | `Pair____i32__boolean` |

Multiple type arguments use `__` (two underscores) between them.

### Nested Generics

Nested generic types are flattened in the mangled name:

```ignis
let nested: Box<Box<i32>>;
```

Uses type: `Box____Box____i32`

## Type Aliases

Type aliases are transparent at the ABI level. They resolve to their target type during compilation and have no runtime representation:

```ignis
type Integer = i32;
type IntBox = Box<i32>;
type Identity<T> = T;

let x: Integer = 42;           // i32 at runtime
let b: IntBox = Box { value: 1 }; // Box____i32 at runtime
let y: Identity<i32> = 10;     // i32 at runtime
```

Generic type aliases substitute their type arguments into the target type before code generation.

## Pointer and Reference Types

| Ignis Type | C Type |
|------------|--------|
| `*T` | `T*` |
| `&T` | `T*` |
| `&mut T` | `T*` |

References and pointers both compile to C pointers. The distinction is enforced at compile time.

## Vector/Array Types

| Ignis Type | C Type |
|------------|--------|
| `T[N]` | `T[N]` (stack allocated) |

## Calling Convention

Ignis uses the standard C calling convention (cdecl on x86, System V AMD64 ABI on x86-64).

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

```ignis
function foo(): void { return; }
```

Compiles to:

```c
void foo(void) { return; }
```

## Entry Point

The `main` function generates `int main(void)`:

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

## String Representation

Strings are null-terminated C strings (`char*`). This has not changed from v0.1.

## Owned Types and Drop Semantics

Same as v0.1. The compiler tracks ownership and emits drop calls at scope exits, early returns, break/continue, and variable overwrites.

## Memory Management

Same as v0.1. See `std::memory` for allocation functions.

## Stability Guarantees

For v0.2, the following are considered stable:

- Type mappings in `types.h`
- Calling convention (C ABI)
- Entry point signature (`int main(void)`)
- Record struct layout (fields as `field_N`)
- Enum tagged union layout (tag + payload union)
- Name mangling scheme (`____` for generics, `_` for namespaces)
- Memory allocation interface

The following may change in future versions:

- String representation
- RTTI type IDs
- Internal codegen details
- Mangling for complex nested types
