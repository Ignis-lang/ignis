#pragma once

#include "../ignis_rt.h"

/**
 * Numeric helper functions used by the std/number module.
 */

/**
 * Returns the absolute value of an i8 as an unsigned magnitude.
 *
 * @param value Signed input.
 * @return Unsigned magnitude of `value`.
 */
u8 i8Abs(i8 value);
/**
 * Returns the absolute value of an i16 as an unsigned magnitude.
 *
 * @param value Signed input.
 * @return Unsigned magnitude of `value`.
 */
u16 i16Abs(i16 value);
/**
 * Returns the absolute value of an i32 as an unsigned magnitude.
 *
 * @param value Signed input.
 * @return Unsigned magnitude of `value`.
 */
u32 i32Abs(i32 value);
/**
 * Returns the absolute value of an i64 as an unsigned magnitude.
 *
 * @param value Signed input.
 * @return Unsigned magnitude of `value` (INT64_MIN maps to 2^63).
 */
u64 i64Abs(i64 value);

/**
 * Returns the absolute value of an f32.
 *
 * @param value Input value.
 * @return Absolute value (NaN stays NaN).
 */
f32 f32Abs(f32 value);
/**
 * Returns the absolute value of an f64.
 *
 * @param value Input value.
 * @return Absolute value (NaN stays NaN).
 */
f64 f64Abs(f64 value);

/**
 * Rounds `value` to `digits` decimal places.
 *
 * @param value Input value.
 * @param digits Decimal digits to keep.
 * @return Rounded value (half away from zero).
 * @note Very large values may be returned unchanged.
 */
f32 f32ToFixed(f32 value, u32 digits);
/**
 * Rounds `value` to `digits` decimal places.
 *
 * @param value Input value.
 * @param digits Decimal digits to keep.
 * @return Rounded value (half away from zero).
 * @note Very large values may be returned unchanged.
 */
f64 f64ToFixed(f64 value, u32 digits);

/**
 * Returns the floor of `value`.
 *
 * @param value Input value.
 * @return Largest integer value not greater than `value`.
 * @note Very large values may be returned unchanged.
 */
f32 f32Floor(f32 value);
/**
 * Returns the floor of `value`.
 *
 * @param value Input value.
 * @return Largest integer value not greater than `value`.
 * @note Very large values may be returned unchanged.
 */
f64 f64Floor(f64 value);

/**
 * Returns the rounded value of `value`.
 *
 * @param value Input value.
 * @return Rounded value (half away from zero).
 * @note Very large values may be returned unchanged.
 */
f32 f32Round(f32 value);
/**
 * Returns the rounded value of `value`.
 *
 * @param value Input value.
 * @return Rounded value (half away from zero).
 * @note Very large values may be returned unchanged.
 */
f64 f64Round(f64 value);

/**
 * Returns the ceiling of `value`.
 *
 * @param value Input value.
 * @return Smallest integer value not less than `value`.
 * @note Very large values may be returned unchanged.
 */
f32 f32Ceil(f32 value);
/**
 * Returns the ceiling of `value`.
 *
 * @param value Input value.
 * @return Smallest integer value not less than `value`.
 * @note Very large values may be returned unchanged.
 */
f64 f64Ceil(f64 value);

/**
 * Number-to-string helpers that return heap-allocated strings.
 *
 * Callers must release the result with ignis_string_drop().
 */

/**
 * Formats an i8 as a decimal string.
 *
 * @param value Input value.
 * @return Newly allocated string.
 */
string i8ToString(i8 value);
/**
 * Formats an i16 as a decimal string.
 *
 * @param value Input value.
 * @return Newly allocated string.
 */
string i16ToString(i16 value);
/**
 * Formats an i32 as a decimal string.
 *
 * @param value Input value.
 * @return Newly allocated string.
 */
string i32ToString(i32 value);
/**
 * Formats an i64 as a decimal string.
 *
 * @param value Input value.
 * @return Newly allocated string.
 */
string i64ToString(i64 value);

/**
 * Formats a u8 as a decimal string.
 *
 * @param value Input value.
 * @return Newly allocated string.
 */
string u8ToString(u8 value);
/**
 * Formats a u16 as a decimal string.
 *
 * @param value Input value.
 * @return Newly allocated string.
 */
string u16ToString(u16 value);
/**
 * Formats a u32 as a decimal string.
 *
 * @param value Input value.
 * @return Newly allocated string.
 */
string u32ToString(u32 value);
/**
 * Formats a u64 as a decimal string.
 *
 * @param value Input value.
 * @return Newly allocated string.
 */
string u64ToString(u64 value);

/**
 * Formats an f32 as a decimal string.
 *
 * @param value Input value.
 * @return Newly allocated string.
 */
string f32ToString(f32 value);
/**
 * Formats an f64 as a decimal string.
 *
 * @param value Input value.
 * @return Newly allocated string.
 */
string f64ToString(f64 value);

/**
 * Returns a newly allocated empty string.
 *
 * @return Newly allocated empty string.
 */
string stringEmpty(void);
