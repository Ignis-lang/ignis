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
extern const u32 TYPE_POINTER_ID;
