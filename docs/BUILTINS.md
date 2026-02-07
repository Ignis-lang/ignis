# Builtins and Compiler Directives

This document describes the builtin functions and compiler directives available in Ignis.

## Builtin Syntax

Builtins use the `@` prefix followed by an identifier and a parenthesized argument list:

```
@name(arg1, arg2, ...)
```

Builtins that operate on types use generic syntax with angle brackets:

```
@name<Type>()
```

Builtins are expressions -- they have a type and return a value (or diverge with `Never`).

### Argument Types

Builtin arguments come in two forms:

- **Expression arguments**: any valid Ignis expression, passed inside `(...)`
- **Type arguments**: types passed via generic syntax `<T>`

```ignis
@sizeOf<i32>()                 // type argument
@configFlag("os.linux")        // expression argument (string literal)
@panic("something went wrong") // expression argument
```

## Builtins Reference

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
function getPlatformName(): string {
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

---

### `@panic(message)`

Terminates program execution immediately.

| | |
|---|---|
| **Arguments** | 1 expression |
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

Current implementation: emits a hardware trap (`__builtin_trap()` in C). The message is validated at compile time but not printed at runtime. Future versions will emit a runtime call that prints the message and a stack trace before aborting.

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
| `@panic("msg")` | Yes (trap) | Yes | No | Logic errors, failed assertions |
| `@trap()` | Yes (trap) | Yes | No | Low-level assertions, impossible states |
| `@unreachable()` | No | No | **Yes** | Optimization hints with formal proof |

In the current implementation, `@panic` and `@trap` emit identical code (`__builtin_trap()`). The semantic difference matters for the future: `@panic` will eventually print its message.

---

## Summary

| Builtin | Arguments | Return type | Emits runtime code |
|---------|-----------|:---:|:---:|
| `@configFlag("key")` | 1 string literal | `boolean` | No (resolved to literal) |
| `@compileError("msg")` | 1 string literal | `Never` | No (compile error) |
| `@sizeOf<T>()` | 1 type arg | `u64` | Yes (`sizeof`) |
| `@alignOf<T>()` | 1 type arg | `u64` | Yes (`_Alignof`) |
| `@panic("msg")` | 1 expression | `Never` | Yes (trap) |
| `@trap()` | none | `Never` | Yes (trap) |
| `@unreachable()` | none | `Never` | No (UB hint) |

## Inline Functions

The `inline` keyword is a modifier for functions and methods. It instructs the compiler to inline the function at call sites.

```ignis
inline function add(a: i32, b: i32): i32 {
    return a + b;
}
```

In methods:

```ignis
record Vector2 {
    x: f64,
    y: f64,

    inline function length(&self): f64 {
        return Math::sqrt(self.x * self.x + self.y * self.y);
    }
}
```

Functions marked `inline` are emitted with the `static inline` specifier in the C backend. The C compiler (gcc/clang) decides whether to actually inline the function body. `inline` is a hint, not a guarantee.

### Where `inline` Can Be Used

| Context | Example |
|---------|---------|
| Top-level functions | `inline function foo(): void { ... }` |
| Record methods | `inline function method(&self): i32 { ... }` inside a record |

`inline` cannot be applied to external functions (`extern`) since their bodies are not available to the compiler.

## Diagnostics

| Code | Message | Severity |
|------|---------|----------|
| A0110 | Unknown builtin '@name' | Error |
| A0111 | *(user-provided message)* | Error |
| A0112 | @name expects N argument(s), got M | Error |
| A0113 | @name expects a string literal argument | Error |
| A0070 | Wrong number of type arguments for '@name': expected N, got M | Error |
| A0115 | Unknown config flag 'key' | Warning |
