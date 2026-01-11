#include "types.h"

// =============================================================================
// Type ID constants (legacy, for backward compatibility)
// =============================================================================

const u32 TYPE_I8_ID = IGNIS_TYPE_I8_ID;
const u32 TYPE_I16_ID = IGNIS_TYPE_I16_ID;
const u32 TYPE_I32_ID = IGNIS_TYPE_I32_ID;
const u32 TYPE_I64_ID = IGNIS_TYPE_I64_ID;
const u32 TYPE_U8_ID = IGNIS_TYPE_U8_ID;
const u32 TYPE_U16_ID = IGNIS_TYPE_U16_ID;
const u32 TYPE_U32_ID = IGNIS_TYPE_U32_ID;
const u32 TYPE_U64_ID = IGNIS_TYPE_U64_ID;
const u32 TYPE_F32_ID = IGNIS_TYPE_F32_ID;
const u32 TYPE_F64_ID = IGNIS_TYPE_F64_ID;
const u32 TYPE_BOOL_ID = IGNIS_TYPE_BOOL_ID;
const u32 TYPE_CHAR_ID = IGNIS_TYPE_CHAR_ID;
const u32 TYPE_STRING_ID = IGNIS_TYPE_STRING_ID;
const u32 TYPE_UNKNOWN_ID = IGNIS_TYPE_UNKNOWN_ID;

// =============================================================================
// IgnisUnknown wrappers
// =============================================================================
// NOTE: typeOf(x) is now a language builtin. For unknown values, the compiler
// generates direct access to the type_id field. No wrapper function needed.

// Constructors
IgnisUnknown unknownFromI8(i8 v) {
    return ignis_unknown_i8(v);
}

IgnisUnknown unknownFromI16(i16 v) {
    return ignis_unknown_i16(v);
}

IgnisUnknown unknownFromI32(i32 v) {
    return ignis_unknown_i32(v);
}

IgnisUnknown unknownFromI64(i64 v) {
    return ignis_unknown_i64(v);
}

IgnisUnknown unknownFromU8(u8 v) {
    return ignis_unknown_u8(v);
}

IgnisUnknown unknownFromU16(u16 v) {
    return ignis_unknown_u16(v);
}

IgnisUnknown unknownFromU32(u32 v) {
    return ignis_unknown_u32(v);
}

IgnisUnknown unknownFromU64(u64 v) {
    return ignis_unknown_u64(v);
}

IgnisUnknown unknownFromF32(f32 v) {
    return ignis_unknown_f32(v);
}

IgnisUnknown unknownFromF64(f64 v) {
    return ignis_unknown_f64(v);
}

IgnisUnknown unknownFromBool(boolean v) {
    return ignis_unknown_bool(v);
}

IgnisUnknown unknownFromString(IgnisString* v) {
    return ignis_unknown_obj(v);
}

IgnisUnknown unknownFromPtr(void* v, u32 typeId) {
    return ignis_unknown_rawptr(v, typeId);
}

// Extractors
i8 unknownAsI8(IgnisUnknown v) {
    return (i8)ignis_unknown_as_i64(v);
}

i16 unknownAsI16(IgnisUnknown v) {
    return (i16)ignis_unknown_as_i64(v);
}

i32 unknownAsI32(IgnisUnknown v) {
    return (i32)ignis_unknown_as_i64(v);
}

i64 unknownAsI64(IgnisUnknown v) {
    return ignis_unknown_as_i64(v);
}

u8 unknownAsU8(IgnisUnknown v) {
    return (u8)ignis_unknown_as_u64(v);
}

u16 unknownAsU16(IgnisUnknown v) {
    return (u16)ignis_unknown_as_u64(v);
}

u32 unknownAsU32(IgnisUnknown v) {
    return (u32)ignis_unknown_as_u64(v);
}

u64 unknownAsU64(IgnisUnknown v) {
    return ignis_unknown_as_u64(v);
}

f32 unknownAsF32(IgnisUnknown v) {
    return ignis_unknown_as_f32(v);
}

f64 unknownAsF64(IgnisUnknown v) {
    return ignis_unknown_as_f64(v);
}

boolean unknownAsBool(IgnisUnknown v) {
    return ignis_unknown_as_u64(v) != 0 ? TRUE : FALSE;
}

IgnisString* unknownAsString(IgnisUnknown v) {
    return (IgnisString*)ignis_unknown_as_ptr(v);
}

void* unknownAsPtr(IgnisUnknown v) {
    return ignis_unknown_as_ptr(v);
}

// Utility
boolean unknownIsNull(IgnisUnknown v) {
    return ignis_unknown_is_null(v);
}

void unknownDrop(IgnisUnknown v) {
    ignis_drop_unknown(v);
}
