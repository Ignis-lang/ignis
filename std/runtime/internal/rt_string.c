/* Ignis runtime: float formatting.  See ignis_rt.h for IgnisString semantics.
 *
 * Everything else that used to live here is now Ignis, in `std/string`. These
 * two conversions stay because they are `snprintf("%g")`, and reproducing its
 * correctly-rounded six-significant-digit output needs exact decimal
 * arithmetic that the standard library does not have. */

#include "../ignis_rt.h"
#include "rt_internal.h"

#define FLOAT_BUF_SIZE 32

// =============================================================================
// String storage internals
//
// `std/string` owns the `IgnisString` buffer. What is left here is only what
// the two conversions below build their results with.
// =============================================================================

static IgnisString ignis_string_with_capacity(size_t cap) {
  IgnisString s;

  if (cap < 1) {
    cap = 1;
  }

  // ignis_alloc aborts on OOM, so data is always valid.
  s.data = (char *)ignis_alloc(cap);
  s.data[0] = '\0';
  s.len = 0;
  s.cap = cap;

  return s;
}

static IgnisString ignis_string_from_len(const char *s, size_t len) {
  size_t cap = len + 1;
  IgnisString str = ignis_string_with_capacity(cap);

  if (s != NULL && len > 0) {
    memcpy(str.data, s, len);
  }

  str.data[len] = '\0';
  str.len = len;

  return str;
}

// =============================================================================
// Float to string conversions
// =============================================================================

IgnisString ignis_f32_to_string(f32 value) {
  char buf[FLOAT_BUF_SIZE];
  int len = snprintf(buf, FLOAT_BUF_SIZE, "%g", (double)value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString ignis_f64_to_string(f64 value) {
  char buf[FLOAT_BUF_SIZE];
  int len = snprintf(buf, FLOAT_BUF_SIZE, "%g", value);

  return ignis_string_from_len(buf, (size_t)len);
}

// =============================================================================
// Output-pointer init functions
//
// These write into a pre-allocated IgnisString through a pointer.  The caller
// may have a struct with extra trailing fields (e.g. __ignis_drop_state) that
// are not touched by these functions.
// =============================================================================

void ignis_string_init_from_f32(IgnisString *out, f32 value) { *out = ignis_f32_to_string(value); }
void ignis_string_init_from_f64(IgnisString *out, f64 value) { *out = ignis_f64_to_string(value); }
