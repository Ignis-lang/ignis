# Builtins and Compiler Directives

This document describes the builtin functions and compiler directives available in Ignis.

## Builtin Syntax

There are two categories of builtins:

**Directive builtins** use the `@` prefix:

```
@name(arg1, arg2, ...)
@name<Type>()
```

**Function builtins** are called like regular functions but are resolved by the compiler:

```
name(arg)
name<Type>()
```

Both categories are expressions -- they have a type and return a value (or diverge with `Never`).

Legacy `__builtin_*` names are deprecated; use directive form (`@read`, `@write`, `@dropInPlace`, `@dropGlue`, etc.).

### Argument Types

Builtin arguments come in two forms:

- **Expression arguments**: any valid Ignis expression, passed inside `(...)`
- **Type arguments**: types passed via generic syntax `<T>`

```ignis
@sizeOf<i32>()                 // type argument
@configFlag("os.linux")        // expression argument (string literal)
@panic("something went wrong") // expression argument
maxOf<i32>()                   // function builtin with type argument
```

---

## Directive Builtins (`@` prefix)

### `@configFlag(key)`

Queries a compile-time configuration flag and returns a boolean literal.

| | |
|---|---|
| **Arguments** | 1 string literal |
| **Returns** | `boolean` |

The flag is resolved at compile time against the compilation context. The result is a literal `true` or `false` in the generated code -- no runtime comparison is emitted.

```ignis
function main(): i32 {
    let flag: boolean = @configFlag("feature.nonexistent");
    if (flag) {
        return 1;
    }
    return 0;
}
```

#### Supported Keys

| Pattern | Example | Meaning |
|---------|---------|---------|
| `os.<name>` | `"os.linux"`, `"os.macos"`, `"os.windows"` | True if the target OS matches |
| `arch.<name>` | `"arch.x86_64"`, `"arch.aarch64"` | True if the target architecture matches |
| `build.debug` | `"build.debug"` | True in debug builds |
| `build.release` | `"build.release"` | True in release builds |
| `feature.<name>` | `"feature.logging"` | True if the named feature is enabled |

If the key does not match any known pattern, the compiler emits a warning and the value resolves to `false`.

#### Platform-Conditional Code

```ignis
function getPlatformName(): str {
    if (@configFlag("os.linux")) {
        return "Linux";
    }
    if (@configFlag("os.macos")) {
        return "macOS";
    }
    return "unknown";
}
```

Since `@configFlag` resolves to a literal boolean, untaken branches become `if (false) { ... }` in the intermediate representation.

---

### `@compileError(message)`

Emits a compile-time error with the given message. The program will not compile if the compiler reaches this expression.

| | |
|---|---|
| **Arguments** | 1 string literal |
| **Returns** | `Never` |

```ignis
function main(): void {
    @compileError("this should not compile");
}
```

Intended for guarding against invalid configurations:

```ignis
function setup(): void {
    if (!@configFlag("os.linux")) {
        @compileError("This module only supports Linux");
    }
    // ... initialization
}
```

> **Note**: In the current implementation, `@compileError` emits its diagnostic during type checking, which runs unconditionally on all AST nodes. This means the error fires even inside dead branches like `if (false) { @compileError(...) }`. Conditional compile-error suppression requires const-evaluation before type checking, which is not yet implemented.

---

### `@sizeOf<T>()`

Returns the size in bytes of a type as laid out by the code generation backend.

| | |
|---|---|
| **Type arguments** | 1 type |
| **Arguments** | none |
| **Returns** | `u64` |

```ignis
function main(): i32 {
    let s: u64 = @sizeOf<i32>();
    return s as i32; // 4
}
```

Emits C `sizeof(T)`.

---

### `@alignOf<T>()`

Returns the alignment in bytes required by a type.

| | |
|---|---|
| **Type arguments** | 1 type |
| **Arguments** | none |
| **Returns** | `u64` |

```ignis
function main(): i32 {
    let a: u64 = @alignOf<i32>();
    return a as i32; // 4
}
```

Emits C `_Alignof(T)`.

---

### `@typeName<T>()`

Returns a human-readable string representation of a type name. Resolved at compile time -- the result is a string literal in the generated code.

| | |
|---|---|
| **Type arguments** | 1 type |
| **Arguments** | none |
| **Returns** | `str` |

```ignis
function main(): void {
    let name: str = @typeName<i32>(); // "i32"
    return;
}
```

---

### `@bitCast<T>(value)`

Reinterprets the bit pattern of a value as another type. Source and target types must have the same byte size.

| | |
|---|---|
| **Type arguments** | 1 type (target) |
| **Arguments** | 1 expression (value to reinterpret) |
| **Returns** | `T` |

```ignis
function floatBits(f: f32): u32 {
    return @bitCast<u32>(f);
}
```

The compiler validates at compile time that `@sizeOf<Source>() == @sizeOf<Target>()`. At runtime, this emits a `memcpy` with a `_Static_assert` for size equality.

---

### `@pointerCast<T>(ptr)`

Converts between pointer types. Both the argument and the type argument must be pointer types.

| | |
|---|---|
| **Type arguments** | 1 pointer type (target) |
| **Arguments** | 1 expression (pointer to cast) |
| **Returns** | `T` |

```ignis
function toBytePtr(ptr: *i32): *u8 {
    return @pointerCast<*u8>(ptr);
}
```

---

### `@integerFromPointer(ptr)`

Converts a pointer to its integer address representation.

| | |
|---|---|
| **Arguments** | 1 expression (pointer) |
| **Returns** | `u64` |

```ignis
function getAddress(ptr: *i32): u64 {
    return @integerFromPointer(ptr);
}
```

---

### `@pointerFromInteger<T>(value)`

Converts an integer address to a pointer. The type argument must be a pointer type.

| | |
|---|---|
| **Type arguments** | 1 pointer type |
| **Arguments** | 1 expression (integer) |
| **Returns** | `T` |

```ignis
function fromAddress(addr: u64): *i32 {
    return @pointerFromInteger<*i32>(addr);
}
```

---

### `@read<T>(ptr)`

Reads a value of type `T` from a mutable pointer.

| | |
|---|---|
| **Type arguments** | 1 type |
| **Arguments** | 1 expression (`*mut T`) |
| **Returns** | `T` |

```ignis
function load(ptr: *mut i32): i32 {
    return @read<i32>(ptr);
}
```

The pointer argument must be exactly `*mut T`. Passing `null` is a compile-time error when statically known.

---

### `@write<T>(ptr, value)`

Writes a value of type `T` to a mutable pointer.

| | |
|---|---|
| **Type arguments** | 1 type |
| **Arguments** | 2 expressions (`*mut T`, `T`) |
| **Returns** | `void` |

```ignis
function store(ptr: *mut i32, value: i32): void {
    @write<i32>(ptr, value);
}
```

The first argument must be exactly `*mut T`. Passing `null` is a compile-time error when statically known.

---

### `@dropInPlace<T>(ptr)`

Runs drop glue for `T` at the given address.

| | |
|---|---|
| **Type arguments** | 1 type |
| **Arguments** | 1 expression (`*mut T`) |
| **Returns** | `void` |

```ignis
function dropValue(ptr: *mut i32): void {
    @dropInPlace<i32>(ptr);
}
```

If `T` does not need drop, this is a no-op.

---

### `@dropGlue<T>()`

Returns a monomorphized drop callback for `T`.

| | |
|---|---|
| **Type arguments** | 1 type |
| **Arguments** | none |
| **Returns** | `(*mut u8) -> void` |

```ignis
extern Runtime {
    function alloc(size: u64, align: u64, dropFn: (*mut u8) -> void): *mut void;
}

function allocBoxed<T>(size: u64, align: u64): *mut void {
    return Runtime::alloc(size, align, @dropGlue<T>());
}
```

This is used by containers and runtimes that need to drop unknown generic payloads through a uniform callback ABI.

---

### `@panic(message)`

Prints a message to stderr and terminates the program with exit code 101.

| | |
|---|---|
| **Arguments** | 1 string literal |
| **Returns** | `Never` |

```ignis
function divide(a: i32, b: i32): i32 {
    if (b == 0) {
        @panic("division by zero");
    }
    return a / b;
}
```

The return type is `Never`, so the compiler knows execution does not continue past a `@panic` call.

Emits `fprintf(stderr, "panic: %s\n", message); exit(101);` in the C backend.

---

### `@trap()`

Emits a hardware trap instruction directly. This is the low-level version of `@panic` with no message.

| | |
|---|---|
| **Arguments** | none |
| **Returns** | `Never` |

```ignis
function mustNotReach(): void {
    @trap();
}
```

Generates `__builtin_trap()` in the C backend, which produces an illegal instruction on most architectures (e.g., `ud2` on x86). The operating system terminates the process with a signal (typically SIGILL or SIGTRAP).

---

### `@unreachable()`

Asserts to the compiler that a point in the program is unreachable. If execution reaches this point, behavior is **undefined**.

| | |
|---|---|
| **Arguments** | none |
| **Returns** | `Never` |

```ignis
function getSign(x: i32): i32 {
    if (x > 0) { return 1; }
    if (x < 0) { return -1; }
    if (x == 0) { return 0; }
    @unreachable();
}
```

Unlike `@trap()`, which forces a predictable crash, `@unreachable()` generates no safety instruction. The optimizer may assume this point is never reached and eliminate code paths accordingly.

Use `@trap()` when you want a safe, debuggable crash. Use `@unreachable()` only when you can prove the point is unreachable.

---

## Comparison: `@panic` vs `@trap` vs `@unreachable`

| Builtin | Emits code | Predictable crash | UB if reached | Use case |
|---------|:---:|:---:|:---:|---------|
| `@panic("msg")` | Yes (fprintf+exit) | Yes | No | Logic errors, failed assertions |
| `@trap()` | Yes (trap) | Yes | No | Low-level assertions, impossible states |
| `@unreachable()` | No | No | **Yes** | Optimization hints with formal proof |

---

## Function Builtins (no `@` prefix)

These are called like regular functions but resolved by the compiler.

### `typeOf(expression)`

Returns a runtime type identifier for a value.

| | |
|---|---|
| **Arguments** | 1 expression |
| **Returns** | `u32` |

```ignis
function main(): i32 {
    let x: i32 = 42;
    let id: u32 = typeOf(x);
    return id as i32;
}
```

Returns a unique numeric identifier for the type of the expression.

---

### `maxOf<T>()`

Returns the maximum representable value for a numeric type.

| | |
|---|---|
| **Type arguments** | 1 numeric type |
| **Arguments** | none |
| **Returns** | `T` |

```ignis
function main(): i32 {
    let max: i32 = maxOf<i32>(); // 2147483647
    return max;
}
```

Supported types: all integer types (`i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`) and floating-point types (`f32`, `f64`).

---

### `minOf<T>()`

Returns the minimum representable value for a numeric type.

| | |
|---|---|
| **Type arguments** | 1 numeric type |
| **Arguments** | none |
| **Returns** | `T` |

```ignis
function main(): i32 {
    let min: i32 = minOf<i32>(); // -2147483648
    return min;
}
```

For unsigned types, `minOf` returns `0`.

---

## Summary

### Directive Builtins

| Builtin | Arguments | Return type | Emits runtime code |
|---------|-----------|:---:|:---:|
| `@configFlag("key")` | 1 string literal | `boolean` | No (resolved to literal) |
| `@compileError("msg")` | 1 string literal | `Never` | No (compile error) |
| `@sizeOf<T>()` | 1 type arg | `u64` | Yes (`sizeof`) |
| `@alignOf<T>()` | 1 type arg | `u64` | Yes (`_Alignof`) |
| `@typeName<T>()` | 1 type arg | `str` | No (resolved to literal) |
| `@bitCast<T>(expr)` | 1 type arg + 1 expr | `T` | Yes (`memcpy`) |
| `@pointerCast<T>(ptr)` | 1 type arg + 1 expr | `T` | Yes (C cast) |
| `@integerFromPointer(ptr)` | 1 pointer expr | `u64` | Yes (C cast) |
| `@pointerFromInteger<T>(int)` | 1 type arg + 1 expr | `T` | Yes (C cast) |
| `@read<T>(ptr)` | 1 type arg + 1 expr | `T` | Yes (load through pointer) |
| `@write<T>(ptr, value)` | 1 type arg + 2 exprs | `void` | Yes (store through pointer) |
| `@dropInPlace<T>(ptr)` | 1 type arg + 1 expr | `void` | Yes (drop glue call) |
| `@dropGlue<T>()` | 1 type arg | `(*mut u8) -> void` | Yes (function pointer constant) |
| `@panic("msg")` | 1 string literal | `Never` | Yes (fprintf + exit) |
| `@trap()` | none | `Never` | Yes (`__builtin_trap`) |
| `@unreachable()` | none | `Never` | No (`__builtin_unreachable`) |

### Function Builtins

| Builtin | Arguments | Return type | Emits runtime code |
|---------|-----------|:---:|:---:|
| `typeOf(expr)` | 1 expression | `u32` | Yes |
| `maxOf<T>()` | 1 type arg | `T` | Yes (C constant) |
| `minOf<T>()` | 1 type arg | `T` | Yes (C constant) |

## Diagnostics

| Code | Message | Severity |
|------|---------|----------|
| A0110 | Unknown builtin '@name' | Error |
| A0111 | *(user-provided message from @compileError)* | Error |
| A0112 | @name expects N argument(s), got M | Error |
| A0113 | @name expects a string literal argument | Error |
| A0070 | Wrong number of type arguments for '@name': expected N, got M | Error |
| A0115 | Unknown config flag 'key' | Warning |
