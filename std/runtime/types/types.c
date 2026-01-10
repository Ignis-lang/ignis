#include "types.h"

// Type ID constants
const u32 TYPE_I8_ID = 0;
const u32 TYPE_I16_ID = 1;
const u32 TYPE_I32_ID = 2;
const u32 TYPE_I64_ID = 3;
const u32 TYPE_U8_ID = 4;
const u32 TYPE_U16_ID = 5;
const u32 TYPE_U32_ID = 6;
const u32 TYPE_U64_ID = 7;
const u32 TYPE_F32_ID = 8;
const u32 TYPE_F64_ID = 9;
const u32 TYPE_BOOL_ID = 10;
const u32 TYPE_CHAR_ID = 11;
const u32 TYPE_STRING_ID = 12;
const u32 TYPE_UNKNOWN_ID = 13;

u32 typeOf(void *value) {
  // Stub implementation - requires RTTI support from compiler
  (void)value;
  return TYPE_UNKNOWN_ID;
}
