#pragma once

// =============================================================================
// Ignis Runtime Types
// =============================================================================
// This header re-exports the core runtime types for backward compatibility.
// All type definitions are now centralized in ignis_rt.h.

#include "../ignis_rt.h"

// =============================================================================
// Type IDs for runtime type checking (legacy constants)
// =============================================================================
// These are provided as extern constants for compatibility with existing code.
// New code should use the IGNIS_TYPE_*_ID macros from ignis_rt.h.

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

// =============================================================================
// IgnisUnknown wrappers for Ignis std/types module
// =============================================================================
// These functions wrap the ignis_unknown_* functions from ignis_rt for use
// in Ignis code.
//
// NOTE: typeOf(x) is now a language builtin. For unknown values, it compiles
// to direct access of the type_id field. No wrapper function needed.

// Constructors - wrap primitive values into IgnisUnknown
IgnisUnknown unknownFromI8(i8 v);
IgnisUnknown unknownFromI16(i16 v);
IgnisUnknown unknownFromI32(i32 v);
IgnisUnknown unknownFromI64(i64 v);
IgnisUnknown unknownFromU8(u8 v);
IgnisUnknown unknownFromU16(u16 v);
IgnisUnknown unknownFromU32(u32 v);
IgnisUnknown unknownFromU64(u64 v);
IgnisUnknown unknownFromF32(f32 v);
IgnisUnknown unknownFromF64(f64 v);
IgnisUnknown unknownFromBool(boolean v);
IgnisUnknown unknownFromString(IgnisString* v);
IgnisUnknown unknownFromPtr(void* v, u32 typeId);

// Extractors - extract typed values from IgnisUnknown (no runtime check)
i8      unknownAsI8(IgnisUnknown v);
i16     unknownAsI16(IgnisUnknown v);
i32     unknownAsI32(IgnisUnknown v);
i64     unknownAsI64(IgnisUnknown v);
u8      unknownAsU8(IgnisUnknown v);
u16     unknownAsU16(IgnisUnknown v);
u32     unknownAsU32(IgnisUnknown v);
u64     unknownAsU64(IgnisUnknown v);
f32     unknownAsF32(IgnisUnknown v);
f64     unknownAsF64(IgnisUnknown v);
boolean unknownAsBool(IgnisUnknown v);
IgnisString* unknownAsString(IgnisUnknown v);
void*   unknownAsPtr(IgnisUnknown v);

// Utility
boolean unknownIsNull(IgnisUnknown v);
void    unknownDrop(IgnisUnknown v);
