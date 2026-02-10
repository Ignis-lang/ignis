# Rc in Ignis

This document explains how to use `Rc<T>` in Ignis and how to provide a custom Rc runtime implementation.

## What `Rc<T>` means in Ignis

`Rc<T>` is a reference-counted shared handle.

- It is **Copy-like** at the language level: assigning/passing/returning an `Rc<T>` does not move the source value.
- It is still **owned** and must be dropped: every live `Rc<T>` variable is released at scope end.
- `Rc::new(value)` is the constructor syntax.

When the compiler lowers `Rc<T>`, it emits calls to four resolved hooks (`alloc`, `get`, `retain`, `release`) instead of relying on a hardcoded runtime layout.

## Hook contract

The analyzer expects exactly one `extern` provider marked with `@lang(rc_runtime)` and this contract:

```ignis
@lang(rc_runtime)
extern RcRuntimeCustom {
    alloc(payload_size: u64, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void;
    get(handle: *mut void): *mut u8;
    retain(handle: *mut void): void;
    release(handle: *mut void): void;
}
```

Function signatures only:

```ignis
alloc(payload_size: u64, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void
get(handle: *mut void): *mut u8
retain(handle: *mut void): void
release(handle: *mut void): void
```

Important details:

- The extern namespace name is **not fixed**. It can be any name.
- The namespace must include `@lang(rc_runtime)`.
- Hook function names (`alloc/get/retain/release`) are fixed.
- The provider must be unique (only one `@lang(rc_runtime)` provider in the compilation).

## Default behavior (std enabled)

With standard library enabled, std provides a default Rc hooks provider in `std/types/mod.ign`, mapped to runtime C symbols via `@externName(...)`.

So regular code just works:

```ignis
function main(): i32 {
    let a: Rc<i32> = Rc::new(10);
    let b: Rc<i32> = a;
    return 0;
}
```

## Custom Rc provider (std=false or override scenario)

If you disable std, you must define your own extern hooks provider in Ignis.

### 1) Declare hooks in Ignis

```ignis
@lang(rc_runtime)
extern RcRuntimeCustom {
    @externName("custom_rc_alloc")
    function alloc(payload_size: u64, payload_align: u64, drop_fn: (*mut u8) -> void): *mut void;

    @externName("custom_rc_get")
    function get(handle: *mut void): *mut u8;

    @externName("custom_rc_retain")
    function retain(handle: *mut void): void;

    @externName("custom_rc_release")
    function release(handle: *mut void): void;
}
```

`@externName(...)` is optional if your C symbol names already match the Ignis function names.

### 2) Implement symbols in C

Example C ABI-compatible implementation:

```c
#include <stdint.h>
#include <stdlib.h>

typedef void (*DropFn)(void *);

typedef struct {
  uint32_t refcount;
  DropFn drop_fn;
  size_t payload_size;
  size_t payload_align;
  unsigned char payload[];
} RcBox;

void *custom_rc_alloc(uint64_t payload_size, uint64_t payload_align, void *drop_fn) {
  (void)payload_align; // implement alignment policy as needed

  RcBox *box = (RcBox *)calloc(1, sizeof(RcBox) + (size_t)payload_size);
  if (!box) return NULL;

  box->refcount = 1;
  box->drop_fn = (DropFn)drop_fn;
  box->payload_size = (size_t)payload_size;
  box->payload_align = (size_t)payload_align;
  return box;
}

void *custom_rc_get(void *handle) {
  RcBox *box = (RcBox *)handle;
  return box->payload;
}

void custom_rc_retain(void *handle) {
  RcBox *box = (RcBox *)handle;
  box->refcount += 1;
}

void custom_rc_release(void *handle) {
  RcBox *box = (RcBox *)handle;
  if (box->refcount == 0) return;

  box->refcount -= 1;
  if (box->refcount == 0) {
    if (box->drop_fn) {
      box->drop_fn(box->payload);
    }
    free(box);
  }
}
```

### 3) Link your runtime

Use your project build configuration to link the C implementation (library/object path, include dirs, flags, etc.).
`build.cc` and `build.cflags` are applied by the driver.

## What the compiler guarantees

- `Rc::new(value)` lowers to:
  1. `alloc(sizeOf(T), alignOf(T), drop_fn_for_T)`
  2. `ptr = get(handle)`
  3. store `value` into `*ptr`
- Copies emit `retain(handle)`.
- Scope drops emit `release(handle)`.
- The compiler generates a drop helper per payload type `T` and passes it to `alloc`.
  - No-op helper if `T` has no drop requirements.
  - Recursive drop glue if `T` contains droppable fields.

## Analyzer diagnostics for Rc hooks

- `A0150`: `Rc<T>` is used but no `@lang(rc_runtime)` provider was found.
- `A0151`: multiple `@lang(rc_runtime)` providers found.
- `A0152`: provider exists but is missing one or more required hooks.
- `A0153`: hook signature mismatch.

## Current limitations

- `@lang(...)` currently recognizes a fixed hook family name set (`rc_runtime`, `string_runtime`, `vector_runtime`, `weak_runtime`).
- `Rc` is modeled as opaque `void*` at C boundary; do not depend on compiler-internal layout assumptions.
