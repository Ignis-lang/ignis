# Ignis ABI Reference (Current)

This document describes the Application Binary Interface for the current Ignis compiler on `main`. It covers how Ignis constructs map to C code, name mangling, memory layout, drop semantics, and linking.

## Changes from v0.2

- **`string` type removed**: The primitive string type is now `str`, which maps to `const char*` in C.
- **`atom` type**: New primitive type mapping to `ignis_atom_t` (`uint32_t`).
- **Traits**: `@implements(Drop)`, `@implements(Clone)`, `@implements(Copy)`, and user-defined traits.
- **Pattern matching**: `match`, `if let`, `while let`, `let else`.
- **Enum tag type**: Changed from smallest-fitting unsigned integer to always `u32`.
- **Drop state field**: Records and enums with `@implements(Drop)` get a `__ignis_drop_state` field.
- **Use-after-drop guards**: Runtime checks on field access and method calls for droppable types.
- **Module-level emission**: Each module is compiled to its own `.c` / `.o` file with a generated header.
- **Name mangling updated**: Monomorphization uses `__` separator; codegen escapes underscores by doubling.
- **Entry point wrapper**: `main` is renamed to `__ignis_user_main`; the compiler generates a C `main(int argc, char** argv)` wrapper that handles `void`, `i32`, and try-capable enum returns.
- **`defer` statement**: Deferred expressions execute at scope exits in LIFO order before automatic drops. Implementation uses static duplication (no runtime defer stack).
- **Implicit integer widening**: Same-sign integers widen implicitly (`u8` → `u32`, `i8` → `i64`). Mixed-sign still requires an explicit cast.
- **Overload mangling**: Return types are now included in the disambiguation suffix when parameter types are identical.
- **New std modules**: `path`, `ffi`, `fs`.

## Type Mapping

### Primitive Types

| Ignis Type | C Type (emitted) | Underlying C Type |
|------------|------------------|-------------------|
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
| `char` | `char` | `char` |
| `str` | `const char*` | `const char*` |
| `atom` | `ignis_atom_t` | `uint32_t` |
| `void` | `void` | `void` |
| `Never` | `void` | `void` |

### Boolean and Null

```c
#define TRUE 1
#define FALSE 0
typedef void* null;
```

### Compound Types

| Ignis Type | C Type |
|------------|--------|
| `*T` | `T*` |
| `*mut T` | `T*` |
| `&T` | `T*` |
| `&mut T` | `T*` |
| `T[N]` | `T[N]` (stack allocated) |
| `(T1, T2, ...)` | `void*` |
| `(T...) -> R` | `void*` |

References and pointers both compile to C pointers. The distinction between `*T` and `*mut T`, and between `&T` and `&mut T`, is enforced at compile time only.

### Type Aliases

Type aliases are transparent. They resolve to their target type during compilation and have no runtime representation.

## Records

Records compile to C structs. Fields are named `field_0`, `field_1`, etc., in declaration order.

```ignis
record Point {
    public x: i32;
    public y: i32;
}
```

```c
struct Point {
    i32 field_0;
    i32 field_1;
};
```

Empty records (zero fields, no Drop) get a dummy field:

```c
struct Empty {
    char _empty;
};
```

### Record Attributes

| Ignis Attribute | C Output |
|-----------------|----------|
| `@packed` | `__attribute__((packed))` |
| `@aligned(N)` | `__attribute__((aligned(N)))` |
| `@aligned(N)` on field | `__attribute__((aligned(N)))` on the field |

### Record Initialization

Records are initialized field-by-field through a pointer:

```c
(ptr)->field_0 = val0;
(ptr)->field_1 = val1;
```

### Record Methods

Instance methods receive a pointer to the receiver as the first parameter. Static methods are mangled with the record name.

```ignis
record Counter {
    public value: i32;

    get(&self): i32 {
        return self.value;
    }

    public static new(start: i32): Counter {
        return Counter { value: start };
    }
}
```

```c
i32 Counter_get(struct Counter* self);
struct Counter Counter_new(i32 a);
```

## Enums

Enums compile to tagged unions. The tag is always `u32`.

```ignis
enum Option<T> {
    SOME(T),
    NONE,
}
```

Monomorphized for `i32`:

```c
struct Option__i32 {
    u32 tag;
    union {
        struct {
            i32 field_0;
        } variant_0;
    } payload;
};

#define Option__i32_SOME 0
#define Option__i32_NONE 1
```

### Tag Type

The tag type is always `u32`, regardless of the number of variants.

### Unit Variants

Variants without payloads do not contribute to the union. If no variant has a payload, the union is omitted entirely.

```ignis
enum Color {
    RED,
    GREEN,
    BLUE,
}
```

```c
struct Color {
    u32 tag;
};

#define Color_RED 0
#define Color_GREEN 1
#define Color_BLUE 2
```

### Enum Initialization

```c
(ptr)->tag = <variant_tag>;
(ptr)->payload.variant_<tag>.field_0 = val0;
```

### Enum Access

- Tag read: `value.tag`
- Payload field: `value.payload.variant_<tag>.field_<index>`
- Unit enum comparison: compares `.tag` fields with `==` or `!=`

## Name Mangling

Name mangling happens in two stages.

### Stage 1: Monomorphization

When generics are specialized, the monomorphizer creates new definitions with mangled names using `__` (double underscore) as the separator. Underscores in the original name are escaped as `_0`.

| Ignis | Monomorphized Name |
|-------|-------------------|
| `identity<i32>` | `identity__i32` |
| `Box<i32>` | `Box__i32` |
| `Pair<i32, boolean>` | `Pair__i32__boolean` |
| `Vec<i32>.push()` | `Vec__i32__push` |

Type encoding in monomorphized names:

| Type | Mangled Form |
|------|-------------|
| Primitives | `i8`, `i16`, `i32`, etc. |
| `*T` | `ptr_<T>` |
| `*mut T` | `ptrmut_<T>` |
| `&T` | `ref_<T>` |
| `&mut T` | `refmut_<T>` |
| `T[N]` | `arrN_<T>` |
| `(T1, T2)` | `tuple_<T1>_<T2>` |
| `(P...) -> R` | `fn_<P1>_<P2>_<R>` |

### Stage 2: Codegen

When emitting C identifiers, the codegen applies its own mangling. Underscores in names are doubled (`_` becomes `__`).

Rules:

1. **`main`** at top level is renamed to `__ignis_user_main` (the compiler generates a C `main` wrapper).
2. **Extern functions** use their raw name, or `@externName("name")` if specified.
3. **Namespaced functions**: `{ns1}_{ns2}_..._{escaped_name}`.
4. **Methods**: `{escaped_owner}_{escaped_method}`.
5. **Overloaded functions**: When multiple functions share the same name, a parameter-type suffix is appended: `{name}_{type1}_{type2}_...`. If two overloads have identical parameter types but different return types, the return type is also appended: `{name}_{param_types}_{return_type}`.

```ignis
namespace Math {
    function add(a: i32, b: i32): i32 {
        return a + b;
    }
}
```

```c
i32 Math_add(i32 a, i32 b);
```

Nested namespaces chain underscores:

```c
i32 Outer_Inner_foo(void);
```

## Calling Convention

Ignis uses the standard C calling convention (cdecl on x86, System V AMD64 ABI on x86-64).

### Function Signatures

```ignis
function add(a: i32, b: i32): i32 {
    return a + b;
}
```

```c
i32 add(i32 a, i32 b) {
    return a + b;
}
```

Functions with no parameters use `(void)` in C. Variadic functions use `(params..., ...)`.

### Entry Point

The user's `main` function is renamed to `__ignis_user_main`. The compiler generates a C `main` wrapper that calls it.

Supported Ignis `main` signatures:

| Ignis Signature | Wrapper Behavior |
|-----------------|-----------------|
| `main(): void` | Calls `__ignis_user_main()`, returns 0 |
| `main(): i32` | Returns the i32 value directly |
| `main(): Result<i32, E>` | Returns OK payload; on ERROR prints the error message and exits 101 |
| `main(argc: i32, argv: *str): ...` | Forwards `(i32)argc, (const char**)argv` |

The wrapper signature is always `int main(int argc, char** argv)`. When `main` takes no parameters, `argc` and `argv` are cast to `(void)`.

```c
// Ignis: function main(): Result<i32, IoError>
struct Result____i32____IoError __ignis_user_main(void);

int main(int argc, char** argv) {
    (void)argc;
    (void)argv;
    struct Result____i32____IoError __ignis_main_result = __ignis_user_main();
    if (__ignis_main_result.tag == 0) {
        return __ignis_main_result.payload.variant_0.field_0;
    }
    fprintf(stderr, "Error: %s\n", __ignis_main_result.payload.variant_1.field_0.field_2);
    exit(101);
}
```

Error display for try-capable returns:

| Error Type | Display |
|------------|---------|
| `str` | Printed directly |
| Record with `message: str` field | Accesses the field |
| Other | Prints `(error)` |

### Visibility

- Non-public, non-extern functions are `static` (internal linkage).
- Public functions have external linkage.
- Monomorphized callees force-emitted into another module are `static`.

### Inline Attributes

| Ignis | C (public) | C (non-public) |
|-------|------------|----------------|
| `@inline` | `inline` | `static inline` |
| `@inline(always)` | `__attribute__((always_inline)) inline` | `__attribute__((always_inline)) static inline` |
| `@inline(never)` | `__attribute__((noinline))` | `__attribute__((noinline))` |
| `@cold` | `__attribute__((cold))` | `__attribute__((cold))` |

### Extern Functions

Extern functions are declared with `extern`. When calling extern functions, pointer and reference arguments are cast through `void*` to avoid C incompatible-pointer-type warnings.

## Drop Semantics

### Drop State

Records and enums with `@implements(Drop)` get an extra field:

```c
struct Resource {
    i32 field_0;
    uint8_t __ignis_drop_state;  // 0 = alive, 1 = dropped
};
```

### Drop Scheduling

The compiler schedules drops at three kinds of program points:

1. **Scope end**: When a block ends, all owned variables are dropped in reverse declaration order.
2. **Early exits**: `return`, `break`, and `continue` drop all owned variables in abandoned scopes before exiting.
3. **Reassignment**: When an owned variable is reassigned, the old value is dropped first.

### Defer Scheduling

`defer` expressions are executed at scope exits **before** automatic drops. Multiple defers in the same scope run in LIFO order (last registered first). The implementation uses static duplication — deferred HIR bodies are re-lowered at each exit point (return, break, continue, scope end), requiring no runtime defer stack.

Execution order at a scope exit:

1. Deferred expressions (LIFO)
2. Drop calls (reverse declaration order)

### Drop Emission

**Types with an explicit `drop` method:**

```c
if (!(expr).__ignis_drop_state) {
    <drop_method>(&expr);
    (expr).__ignis_drop_state = 1;
}
```

**Types without `drop` but with droppable fields:**

The compiler recursively emits drops for each field that needs dropping. For enums, a `switch` on the tag drops only the active variant's payload fields.

### Drop Glue

`@dropGlue<T>()` generates a static helper function:

```c
static void ignis_drop_glue_<mangled_type>(u8* payload) {
    <type>* __dip = (<type>*)payload;
    // recursive field drops
}
```

This returns a function pointer `void(*)(u8*)` for use by containers that need to drop generic payloads through a uniform callback.

### Use-After-Drop Guards

Field access and method calls on droppable types emit runtime checks:

```c
if ((base)->__ignis_drop_state) {
    fprintf(stderr, "panic: use of dropped value\n");
    exit(101);
}
```

### Copy vs Drop

Copy and Drop are mutually exclusive. A type cannot be both.

- **Copy types**: Primitives, pointers, references, function types, and records/enums where all fields are Copy.
- **Non-Copy types**: `str`, types with `@implements(Drop)`, types with any non-Copy field.

## Pointer and Cast Operations

### Pointer-Integer Casts

Pointer-to-integer and integer-to-pointer casts go through `uintptr_t` for C standards compliance:

```c
// ptr -> int
target = (target_type)(uintptr_t)(ptr);

// int -> ptr
target = (target_type)(uintptr_t)(int_val);
```

### BitCast

`@bitCast<T>(value)` uses `memcpy` with a compile-time size check:

```c
_Static_assert(sizeof(target_type) == sizeof(source_type), "bitCast: size mismatch");
memcpy(&dest, &source, sizeof(target_type));
```

### Vector Assignment

Fixed-size array assignment uses `memcpy`:

```c
memcpy(dest, source, sizeof(element_type) * N);
```

## C File Emission Order

The generated C file is structured in this order:

1. **Implicit headers** (`<stdio.h>`, `<math.h>`, `<string.h>` based on usage)
2. **Explicit headers** (from the link plan)
3. **Type forward declarations** (`typedef struct X X;`)
4. **Type definitions** (full struct/union bodies)
5. **Static constants** (record/enum static fields)
6. **Extern declarations** (runtime function prototypes)
7. **Drop glue helpers** (`static void ignis_drop_glue_<type>(u8*)`)
8. **Forward declarations** (function prototypes)
9. **Function bodies**
10. **Entry point wrapper** (`int main(int argc, char** argv)` — only in the entry module)

All definitions are sorted by `DefinitionId` index for deterministic output.

## Module Headers

Each module generates a `.h` file with:

- Include guard (`#ifndef` / `#define`)
- `#include "runtime/ignis_rt.h"`
- Forward declarations for external struct types
- Full struct definitions (guarded by `#ifndef IGNIS_TYPE_DEF_<NAME>` to avoid redefinition)
- Function prototypes for public, non-extern, non-generic functions and methods

## Linking

### Compilation Pipeline

1. C source is emitted to `.c` files.
2. **Compile to object**: `gcc -c <file.c> -o <file.o> [-I <include_dirs>] [<cflags>]`
3. **Archive creation** (for libraries): `ar rcs <archive.a> <obj1.o> <obj2.o> ...`
4. **Link executable**: `gcc <user.o> [<archives>] <runtime_objects> -o <binary> [-l<lib>]`

### Link Order

1. All user object files
2. User archive (`libignis_user.a`) if present
3. Std archive (`libignis_std.a`) if present
4. Runtime objects (`libignis_rt.a`, module-specific objects)
5. Output binary (`-o`)
6. External library flags (`-lm`, etc.)

### Archive Layout

| Archive | Location | Contents |
|---------|----------|----------|
| Runtime | `std/runtime/libignis_rt.a` | Core C runtime |
| Std | `build/std/lib/libignis_std.a` | Compiled std modules |
| User | `build/user/lib/libignis_user.a` | Compiled user modules |

### Precompiled Std

The linker checks for `build/std/include/ignis_std.h` and `build/std/lib/libignis_std.a`. If both exist, it uses the precompiled std and links all runtime objects from the manifest.

### Standard Library Manifest

The module registry at `std/manifest.toml` declares:

- **Modules**: `libc`, `io`, `math`, `string`, `number`, `types`, `option`, `result`, `memory`, `vector`, `ptr`, `rc`, `path`, `ffi`, `fs`
- **Auto-loaded modules** (imported implicitly): `string`, `number`, `vector`, `types`, `option`, `result`
- **Runtime linking**: `runtime/libignis_rt.a` with header `runtime/ignis_rt.h`
- **System libraries**: `math` links `-lm`

## Definition Classification

The codegen classifies every definition into one of three categories to decide what gets emitted in each translation unit:

| Category | Description |
|----------|-------------|
| `User` | User-written code from project `.ign` files |
| `Std(module)` | Standard library code, tagged by module name |
| `Runtime` | Extern C functions from runtime headers |

Classification rules:

1. Extern definitions are always `Runtime`.
2. Definitions from std module paths are `Std(module_name)`.
3. Everything else is `User`.

When emitting a user module, monomorphized callees from other modules are force-emitted if reachable and not extern.

## Stability Guarantees

The following are considered stable:

- Type mappings in the runtime header
- Calling convention (C ABI)
- Entry point wrapper (`int main(int argc, char** argv)` calling `__ignis_user_main`)
- Record struct layout (`field_N` naming)
- Enum tagged union layout (tag + payload union)
- Drop state field placement (`__ignis_drop_state`)

The following may change:

- Name mangling scheme details
- Overload disambiguation suffixes
- Drop glue function naming
- Module header guard naming
- RTTI type IDs
