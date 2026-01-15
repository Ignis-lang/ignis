#include "number.h"
#include <float.h>
#include <stdio.h>

#define INT_BUF_SIZE 24
#define FLOAT_BUF_SIZE 32

// =============================================================================
// Numeric helpers
// =============================================================================

static f64 f64_floor_impl(f64 value) {
  if (value != value) {
    return value;
  }

  if (value >= (f64)INT64_MAX || value <= (f64)INT64_MIN) {
    return value;
  }

  i64 truncated = (i64)value;
  f64 truncated_f = (f64)truncated;

  if (value < 0.0 && truncated_f != value) {
    return truncated_f - 1.0;
  }

  return truncated_f;
}

static f64 f64_ceil_impl(f64 value) {
  if (value != value) {
    return value;
  }

  if (value >= (f64)INT64_MAX || value <= (f64)INT64_MIN) {
    return value;
  }

  i64 truncated = (i64)value;
  f64 truncated_f = (f64)truncated;

  if (value > 0.0 && truncated_f != value) {
    return truncated_f + 1.0;
  }

  return truncated_f;
}

static f64 f64_round_impl(f64 value) {
  if (value != value) {
    return value;
  }

  if (value >= (f64)INT64_MAX || value <= (f64)INT64_MIN) {
    return value;
  }

  if (value >= 0.0) {
    return f64_floor_impl(value + 0.5);
  }

  return f64_ceil_impl(value - 0.5);
}

static f64 f64_pow10(u32 digits) {
  f64 factor = 1.0;

  for (u32 i = 0; i < digits; i++) {
    if (factor > DBL_MAX / 10.0) {
      return factor;
    }

    factor *= 10.0;
  }

  return factor;
}

u8 i8Abs(i8 value) {
  if (value >= 0) {
    return (u8)value;
  }

  return (u8)(-(i16)value);
}

u16 i16Abs(i16 value) {
  if (value >= 0) {
    return (u16)value;
  }

  return (u16)(-(i32)value);
}

u32 i32Abs(i32 value) {
  if (value >= 0) {
    return (u32)value;
  }

  return (u32)(-(i64)value);
}

u64 i64Abs(i64 value) {
  if (value >= 0) {
    return (u64)value;
  }

  return (u64)(-(value + 1)) + 1;
}

f32 f32Abs(f32 value) {
  if (value < 0.0f) {
    return -value;
  }

  return value;
}

f64 f64Abs(f64 value) {
  if (value < 0.0) {
    return -value;
  }

  return value;
}

f32 f32Floor(f32 value) { return (f32)f64_floor_impl((f64)value); }

f64 f64Floor(f64 value) { return f64_floor_impl(value); }

f32 f32Round(f32 value) { return (f32)f64_round_impl((f64)value); }

f64 f64Round(f64 value) { return f64_round_impl(value); }

f32 f32Ceil(f32 value) { return (f32)f64_ceil_impl((f64)value); }

f64 f64Ceil(f64 value) { return f64_ceil_impl(value); }

f32 f32ToFixed(f32 value, u32 digits) {
  return (f32)f64ToFixed((f64)value, digits);
}

f64 f64ToFixed(f64 value, u32 digits) {
  if (value != value) {
    return value;
  }

  if (value > DBL_MAX || value < -DBL_MAX) {
    return value;
  }

  f64 factor = f64_pow10(digits);
  if (factor == 0.0) {
    return value;
  }

  if (value > DBL_MAX / factor || value < -DBL_MAX / factor) {
    return value;
  }

  f64 scaled = value * factor;
  f64 rounded = f64_round_impl(scaled);

  return rounded / factor;
}

// =============================================================================
// Integer to string conversions
// =============================================================================

string i8ToString(i8 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%d", (int)value);
  return ignis_string_from_len(buf, (size_t)len);
}

string i16ToString(i16 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%d", (int)value);
  return ignis_string_from_len(buf, (size_t)len);
}

string i32ToString(i32 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%d", value);
  return ignis_string_from_len(buf, (size_t)len);
}

string i64ToString(i64 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%ld", value);
  return ignis_string_from_len(buf, (size_t)len);
}

string u8ToString(u8 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%u", (unsigned int)value);
  return ignis_string_from_len(buf, (size_t)len);
}

string u16ToString(u16 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%u", (unsigned int)value);
  return ignis_string_from_len(buf, (size_t)len);
}

string u32ToString(u32 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%u", value);
  return ignis_string_from_len(buf, (size_t)len);
}

string u64ToString(u64 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%lu", value);
  return ignis_string_from_len(buf, (size_t)len);
}

// =============================================================================
// Float to string conversions
// =============================================================================

string f32ToString(f32 value) {
  char buf[FLOAT_BUF_SIZE];
  int len = snprintf(buf, FLOAT_BUF_SIZE, "%g", (double)value);
  return ignis_string_from_len(buf, (size_t)len);
}

string f64ToString(f64 value) {
  char buf[FLOAT_BUF_SIZE];
  int len = snprintf(buf, FLOAT_BUF_SIZE, "%g", value);
  return ignis_string_from_len(buf, (size_t)len);
}
