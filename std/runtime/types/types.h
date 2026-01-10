#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <string.h>

// Unsigned types
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

// Signed types
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

// Floating point types
typedef float f32;
typedef double f64;

// Boolean type
typedef u8 boolean;

// String type
typedef char *string;

// Null pointer type
typedef void *null;

#define TRUE 1
#define FALSE 0

// Type IDs for runtime type checking
extern const u32 TYPE_I8_ID;
extern const u32 TYPE_I16_ID;
extern const u32 TYPE_I32_ID;
extern const u32 TYPE_I64_ID;
extern const u32 TYPE_U8_ID;
extern const u32 TYPE_U16_ID;
extern const u32 TYPE_U32_ID;
extern const u32 TYPE_U64_ID;
extern const u32 TYPE_F32_ID;
extern const u32 TYPE_F64_ID;
extern const u32 TYPE_BOOL_ID;
extern const u32 TYPE_CHAR_ID;
extern const u32 TYPE_STRING_ID;
extern const u32 TYPE_UNKNOWN_ID;

// Runtime type checking (stub - requires RTTI)
u32 typeOf(void *value);
