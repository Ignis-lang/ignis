/* Ignis runtime: string management and conversions.  See ignis_rt.h for IgnisString semantics. */

#include "../ignis_rt.h"
#include "rt_internal.h"

#define INT_BUF_SIZE 24
#define FLOAT_BUF_SIZE 32

// =============================================================================
// String storage internals
//
// `std/string` owns the `IgnisString` buffer. What is left here is only what
// the number conversions below build their results with.
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
// Number to string conversions
// =============================================================================

IgnisString ignis_i8_to_string(i8 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%d", (int)value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString ignis_i16_to_string(i16 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%d", (int)value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString ignis_i32_to_string(i32 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%d", value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString ignis_i64_to_string(i64 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%ld", value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString ignis_u8_to_string(u8 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%u", (unsigned int)value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString ignis_u16_to_string(u16 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%u", (unsigned int)value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString ignis_u32_to_string(u32 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%u", value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString ignis_u64_to_string(u64 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%lu", value);

  return ignis_string_from_len(buf, (size_t)len);
}

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

void ignis_string_init_from_i8(IgnisString *out, i8 value) { *out = ignis_i8_to_string(value); }
void ignis_string_init_from_i16(IgnisString *out, i16 value) { *out = ignis_i16_to_string(value); }
void ignis_string_init_from_i32(IgnisString *out, i32 value) { *out = ignis_i32_to_string(value); }
void ignis_string_init_from_i64(IgnisString *out, i64 value) { *out = ignis_i64_to_string(value); }
void ignis_string_init_from_u8(IgnisString *out, u8 value) { *out = ignis_u8_to_string(value); }
void ignis_string_init_from_u16(IgnisString *out, u16 value) { *out = ignis_u16_to_string(value); }
void ignis_string_init_from_u32(IgnisString *out, u32 value) { *out = ignis_u32_to_string(value); }
void ignis_string_init_from_u64(IgnisString *out, u64 value) { *out = ignis_u64_to_string(value); }
void ignis_string_init_from_f32(IgnisString *out, f32 value) { *out = ignis_f32_to_string(value); }
void ignis_string_init_from_f64(IgnisString *out, f64 value) { *out = ignis_f64_to_string(value); }
