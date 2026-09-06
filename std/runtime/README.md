# Ignis Runtime

## Overview
The runtime is a small C library that defines core types and the C-facing API
used by the Ignis standard library. It provides memory allocation, strings,
dynamic buffers, and small I/O helpers.

**Note**: Language features (records, enums, generics, traits, type aliases) compile to
standard C constructs and do not require additional runtime support.

## Build
The build is driven by `std/runtime/Makefile`. The `all` target produces:

- `libignis_rt.o` (core runtime)
- `io/libignis_io.o`
- `string/libignis_string.o`
- `number/libignis_number.o`
- `types/libignis_types.o`
- `memory/libignis_memory.o`

All module objects depend on `libignis_rt.o`. The `clean` target removes the
objects above.

## Files and functions

### `std/runtime/ignis_rt.h` and `std/runtime/ignis_rt.c`
Core runtime types, IDs, and APIs.

- Types and IDs:
  - `u8`, `u16`, `u32`, `u64`, `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `boolean`
  - `IgnisTypeId`
  - `IgnisHeader`, `IgnisString`
  - `null`
  - `IGNIS_TYPE_*_ID` macros (primitives, pointer)
- Alloc/Free:
  - `ignis_alloc`, `ignis_realloc`, `ignis_free`, defined in `std/memory/allocator.ign`
- String API still in C:
  - `ignis_string_cstr`, `ignis_string_char_at`, `ignis_string_byte_at`
  - `ignis_string_compare`, `ignis_string_index_of`, `ignis_string_contains`
  - `ignis_string_concat`, `ignis_string_substring`, `ignis_string_to_upper`, `ignis_string_to_lower`
  - the `ignis_string_init_*` output-pointer variants for the four above
  - `ignis_{i8,i16,i32,i64,u8,u16,u32,u64,f32,f64}_to_string` and their
    `ignis_string_init_from_*` variants
- String API now in Ignis, in `std/string/mod.ign`:
  - construction and capacity: `String::new`, `String::withCapacity`, `String::create`
  - mutation: `String::pushChar`, `String::pushByte`, `String::pushStr`, `String::push`,
    `String::clear`, `String::reserve`
  - reads: `String::length`, `String::byteAt`
  - copies and release: `String::clone`, `String::drop`

### `std/runtime/memory/memory.h` and `std/runtime/memory/memory.c`
Wrappers used by `std/memory/mod.ign`.

- Allocation:
  - `memoryAllocate`, `memoryDeallocate`, `memoryReallocate`, `memoryAllocateZeroed`

### `std/runtime/number/number.h` and `std/runtime/number/number.c`
Numeric helpers and conversions used by `std/number` and `std/string`.

- Absolute value: `i8Abs`, `i16Abs`, `i32Abs`, `i64Abs`, `f32Abs`, `f64Abs`
- Rounding: `f32Floor`, `f64Floor`, `f32Ceil`, `f64Ceil`, `f32Round`, `f64Round`
- Fixed decimals: `f32ToFixed`, `f64ToFixed`
- Integers to string: `i8ToString`, `i16ToString`, `i32ToString`, `i64ToString`
- Unsigned to string: `u8ToString`, `u16ToString`, `u32ToString`, `u64ToString`
- Floats to string: `f32ToString`, `f64ToString`
- Boolean to string: `booleanToString`
- Utility: `stringEmpty`

All string conversion helpers return heap-allocated `IgnisString` and must be
released with `String::drop` from `std/string`.

### `std/runtime/string/string.h` and `std/runtime/string/string.c`
String operations built on `IgnisString`.

- Length and comparison: `stringLength`, `stringCompare`
- Construction: `stringConcat`, `stringSubstring`
- Character access: `stringCharAt`
- Search: `stringIndexOf`, `stringContains`
- Case conversion: `stringToUpperCase`, `stringToLowerCase`
- Mutation: `stringPushChar`

### `std/runtime/types/types.h` and `std/runtime/types/types.c`
Legacy type IDs for std/types.

- Type ID constants: `TYPE_I8_ID`, `TYPE_I16_ID`, `TYPE_I32_ID`, `TYPE_I64_ID`,
  `TYPE_U8_ID`, `TYPE_U16_ID`, `TYPE_U32_ID`, `TYPE_U64_ID`, `TYPE_F32_ID`,
  `TYPE_F64_ID`, `TYPE_BOOL_ID`, `TYPE_CHAR_ID`, `TYPE_STRING_ID`,
  `TYPE_POINTER_ID`

### `std/runtime/types/primitives.h`
Compatibility header that re-exports `ignis_rt.h` and `string/string.h`.

### `std/runtime/io/io.h` and `std/runtime/io/io.c`
String-based output helpers.

- `print`
- `eprint`
