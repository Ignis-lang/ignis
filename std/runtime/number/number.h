#pragma once

#include "../ignis_rt.h"

// =============================================================================
// Number to string conversions
// =============================================================================
// All functions return a new heap-allocated IgnisString*.
// Caller is responsible for calling ignis_string_drop() when done.

// Integer to string conversions
string i8ToString(i8 value);
string i16ToString(i16 value);
string i32ToString(i32 value);
string i64ToString(i64 value);

string u8ToString(u8 value);
string u16ToString(u16 value);
string u32ToString(u32 value);
string u64ToString(u64 value);

// Float to string conversions
string f32ToString(f32 value);
string f64ToString(f64 value);

// Boolean to string
string booleanToString(boolean value);

// Empty string (for error cases)
string stringEmpty(void);
