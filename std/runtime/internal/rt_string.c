/* Ignis runtime: string management and conversions.  See ignis_rt.h for IgnisString semantics. */

#include "../ignis_rt.h"
#include "rt_internal.h"

#define INT_BUF_SIZE 24
#define FLOAT_BUF_SIZE 32

static ignis_char_t ignis_utf8_replacement_char(void) {
  return 0xFFFDu;
}

static ignis_char_t ignis_utf8_normalize_scalar(ignis_char_t scalar) {
  if ((scalar >= 0xD800u && scalar <= 0xDFFFu) || scalar > 0x10FFFFu) {
    return ignis_utf8_replacement_char();
  }

  return scalar;
}

static size_t ignis_utf8_encode_scalar(ignis_char_t scalar, u8 out[4]) {
  ignis_char_t normalized = ignis_utf8_normalize_scalar(scalar);

  if (normalized <= 0x7Fu) {
    out[0] = (u8)normalized;
    return 1;
  }

  if (normalized <= 0x7FFu) {
    out[0] = (u8)(0xC0u | (normalized >> 6));
    out[1] = (u8)(0x80u | (normalized & 0x3Fu));
    return 2;
  }

  if (normalized <= 0xFFFFu) {
    out[0] = (u8)(0xE0u | (normalized >> 12));
    out[1] = (u8)(0x80u | ((normalized >> 6) & 0x3Fu));
    out[2] = (u8)(0x80u | (normalized & 0x3Fu));
    return 3;
  }

  out[0] = (u8)(0xF0u | (normalized >> 18));
  out[1] = (u8)(0x80u | ((normalized >> 12) & 0x3Fu));
  out[2] = (u8)(0x80u | ((normalized >> 6) & 0x3Fu));
  out[3] = (u8)(0x80u | (normalized & 0x3Fu));
  return 4;
}
// =============================================================================
// Internal helpers
// =============================================================================

static void ignis_string_grow(IgnisString *s, size_t min_cap) {
  size_t new_cap = s->cap;
  if (new_cap == 0) {
    new_cap = 16;
  }

  while (new_cap < min_cap) {
    new_cap *= 2;
  }

  // ignis_realloc aborts on OOM, so no null check needed.
  s->data = (char *)ignis_realloc(s->data, new_cap);
  s->cap = new_cap;
}
// =============================================================================
// String storage internals
//
// The exported construction, mutation and destruction API now lives in
// `std/string`. What is left here is what the still-C operations below build
// their results with.
// =============================================================================

static IgnisString ignis_string_with_capacity(size_t cap);

static IgnisString ignis_string_new(void) {
  return ignis_string_with_capacity(16);
}

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

static void ignis_string_push_char(IgnisString *s, ignis_char_t c) {
  if (s == NULL) {
    return;
  }

  u8 encoded[4];
  size_t encoded_len = ignis_utf8_encode_scalar(c, encoded);
  size_t required = s->len + encoded_len + 1;
  if (required > s->cap) {
    ignis_string_grow(s, required);
  }

  for (size_t index = 0; index < encoded_len; index++) {
    s->data[s->len + index] = (char)encoded[index];
  }

  s->len += encoded_len;
  s->data[s->len] = '\0';
}

static void ignis_string_push_str(IgnisString *s, const IgnisString *other) {
  if (s == NULL || other == NULL || other->len == 0) {
    return;
  }

  size_t required = s->len + other->len + 1;
  if (required > s->cap) {
    ignis_string_grow(s, required);
  }

  memcpy(s->data + s->len, other->data, other->len);
  s->len += other->len;
  s->data[s->len] = '\0';
}

static const char *ignis_string_cstr(const IgnisString *s) {
  if (s == NULL || s->data == NULL) {
    return "";
  }

  return s->data;
}

static size_t ignis_string_len(const IgnisString *s) {
  if (s == NULL) {
    return 0;
  }

  return s->len;
}
// =============================================================================
// String operations
// =============================================================================
IgnisString ignis_string_concat(const IgnisString *a, const IgnisString *b) {
  IgnisString result = ignis_string_new();

  if (a != NULL) {
    ignis_string_push_str(&result, a);
  }

  if (b != NULL) {
    ignis_string_push_str(&result, b);
  }

  return result;
}

IgnisString ignis_string_substring(const IgnisString *s, i64 start, i64 len) {
  if (s == NULL || len <= 0) {
    return ignis_string_new();
  }

  size_t s_len = ignis_string_len(s);
  const char *s_data = ignis_string_cstr(s);

  if (start < 0) {
    start = 0;
  }

  if ((size_t)start >= s_len) {
    return ignis_string_new();
  }

  size_t actual_len = (size_t)len;
  if ((size_t)start + actual_len > s_len) {
    actual_len = s_len - (size_t)start;
  }

  return ignis_string_from_len(s_data + start, actual_len);
}

IgnisString ignis_string_to_upper(const IgnisString *s) {
  if (s == NULL) {
    return ignis_string_new();
  }

  size_t len = ignis_string_len(s);
  const char *data = ignis_string_cstr(s);

  IgnisString result = ignis_string_with_capacity(len + 1);

  for (size_t i = 0; i < len; i++) {
    ignis_string_push_char(&result, (char)toupper((unsigned char)data[i]));
  }

  return result;
}

IgnisString ignis_string_to_lower(const IgnisString *s) {
  if (s == NULL) {
    return ignis_string_new();
  }

  size_t len = ignis_string_len(s);
  const char *data = ignis_string_cstr(s);

  IgnisString result = ignis_string_with_capacity(len + 1);

  for (size_t i = 0; i < len; i++) {
    ignis_string_push_char(&result, (char)tolower((unsigned char)data[i]));
  }

  return result;
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
void ignis_string_init_concat(IgnisString *out, const IgnisString *a, const IgnisString *b) {
  *out = ignis_string_concat(a, b);
}

void ignis_string_init_substring(IgnisString *out, const IgnisString *s, i64 start, i64 len) {
  *out = ignis_string_substring(s, start, len);
}

void ignis_string_init_to_upper(IgnisString *out, const IgnisString *s) {
  *out = ignis_string_to_upper(s);
}

void ignis_string_init_to_lower(IgnisString *out, const IgnisString *s) {
  *out = ignis_string_to_lower(s);
}

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
