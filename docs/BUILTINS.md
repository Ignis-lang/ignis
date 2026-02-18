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
@panic("something went wrong") // expression argument
maxOf<i32>()                   // function builtin with type argument
```

---

## Directive Builtins (`@` prefix)

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

Use inside a `@configFlag` or `@if` false branch to reject unsupported configurations:

```ignis
@configFlag(!@platform("linux"))
function linuxOnly(): void {
    @compileError("This module only supports Linux");
}
```

> **Note**: `@compileError` emits its diagnostic during type checking, which runs on all reachable AST nodes. Items excluded by `@configFlag` or `@if` are stripped at parse time and never reach the analyzer, so the error only fires when the item is actually included.

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

## Compile-Time Directives

These are parser-level constructs that conditionally include or exclude declarations and statements. They are resolved at parse time based on the compilation context (target triple, features, build mode).

### `@configFlag(condition)`

Works in two contexts:

**Item attribute** — conditionally includes the next declaration. If the condition is false, the item is entirely skipped at parse time.

```ignis
@configFlag(@platform("linux"))
extern __errno {
  @externName("__errno_location")
  function errno_location(): *mut i32;
}

@configFlag(@platform("macos"))
extern __errno {
  @externName("__error")
  function errno_location(): *mut i32;
}
```

Valid on: any top-level declaration, namespace item, or block statement.

**Expression** — evaluates to a `boolean` literal (`true` or `false`) at parse time. Useful for conditional logic inside function bodies.

```ignis
function getPlatformName(): str {
    if (@configFlag(@platform("linux"))) {
        return "Linux";
    }
    if (@configFlag(@platform("macos"))) {
        return "macOS";
    }
    return "unknown";
}
```

Since the result is a literal boolean, untaken branches become `if (false) { ... }` in the generated code.

### `@if(condition) { ... } @else { ... }`

Conditionally includes one of two blocks of declarations/statements.

```ignis
@if(@platform("linux") && @arch("x86_64")) {
  function getPageSize(): u64 { return 4096; }
} @else {
  function getPageSize(): u64 { return 16384; }
}
```

The `@else` branch is optional. Without it, the block is simply omitted when the condition is false.

### `@ifelse(condition) { ... } @else { ... }`

Same as `@if`, but the `@else` branch is **mandatory**. Compile error if omitted.

### Condition Predicates

| Predicate | Example | Matches |
|-----------|---------|----------|
| `@platform("name")` | `@platform("linux")` | Target OS |
| `@arch("name")` | `@arch("x86_64")` | Target architecture |
| `@abi("name")` | `@abi("gnu")` | Target ABI/environment |
| `@target("triple")` | `@target("x86_64-unknown-linux-gnu")` | Exact target triple match |
| `@feature("name")` | `@feature("simd")` | Enabled feature flag |
| `@debug()` | `@debug()` | True in debug builds |
| `@release()` | `@release()` | True in release builds |

### Boolean Combinators

Conditions support `&&`, `||`, `!`, and parentheses:

```ignis
@configFlag(@platform("linux") && @arch("x86_64"))
function optimized(): void { }

@if(!@platform("windows") || @feature("force_posix")) {
  function usePosixApi(): void { }
}
```

### Feature Validation

If `known_features` is declared in `ignis.toml`, using an undeclared feature name in `@feature("...")` is a compile error. Without `known_features`, any name is accepted.

```toml
[build]
known_features = ["simd", "logging"]
default_features = ["logging"]
```

Features are enabled via CLI: `ignis build --feature simd` or `--features simd,logging`.

### Target Triple Priority

The target triple used by predicates follows: **CLI** (`--target-triple`) > **ignis.toml** (`target_triple`) > **host** (auto-detected).

---

## Diagnostics

| Code | Message | Severity |
|------|---------|----------|
| A0110 | Unknown builtin '@name' | Error |
| A0111 | *(user-provided message from @compileError)* | Error |
| A0112 | @name expects N argument(s), got M | Error |
| A0113 | @name expects a string literal argument | Error |
| A0070 | Wrong number of type arguments for '@name': expected N, got M | Error |
