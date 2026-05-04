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

static bool ignis_utf8_decode_at(
  const IgnisString *s,
  size_t idx,
  ignis_char_t *out_scalar,
  size_t *out_end
) {
  if (out_end != NULL) {
    *out_end = idx;
  }

  if (out_scalar != NULL) {
    *out_scalar = 0;
  }

  if (s == NULL || s->data == NULL || idx >= s->len) {
    return false;
  }

  const u8 *data = (const u8 *)s->data;
  u8 first = data[idx];

  if (first <= 0x7Fu) {
    if (out_scalar != NULL) {
      *out_scalar = (ignis_char_t)first;
    }
    if (out_end != NULL) {
      *out_end = idx + 1;
    }
    return true;
  }

  if (first >= 0x80u && first <= 0xBFu) {
    return false;
  }

  if (first >= 0xC2u && first <= 0xDFu) {
    if (idx + 1 >= s->len) {
      return false;
    }

    u8 second = data[idx + 1];
    if (second < 0x80u || second > 0xBFu) {
      return false;
    }

    if (out_scalar != NULL) {
      *out_scalar = (ignis_char_t)(((first & 0x1Fu) << 6) | (second & 0x3Fu));
    }
    if (out_end != NULL) {
      *out_end = idx + 2;
    }
    return true;
  }

  if (first >= 0xE0u && first <= 0xEFu) {
    if (idx + 2 >= s->len) {
      return false;
    }

    u8 second = data[idx + 1];
    u8 third = data[idx + 2];
    if (second < 0x80u || second > 0xBFu || third < 0x80u || third > 0xBFu) {
      return false;
    }
    if (first == 0xE0u && second < 0xA0u) {
      return false;
    }
    if (first == 0xEDu && second >= 0xA0u) {
      return false;
    }

    if (out_scalar != NULL) {
      *out_scalar = (ignis_char_t)(((first & 0x0Fu) << 12) | ((second & 0x3Fu) << 6) | (third & 0x3Fu));
    }
    if (out_end != NULL) {
      *out_end = idx + 3;
    }
    return true;
  }

  if (first >= 0xF0u && first <= 0xF4u) {
    if (idx + 3 >= s->len) {
      return false;
    }

    u8 second = data[idx + 1];
    u8 third = data[idx + 2];
    u8 fourth = data[idx + 3];
    if (
      second < 0x80u || second > 0xBFu ||
      third < 0x80u || third > 0xBFu ||
      fourth < 0x80u || fourth > 0xBFu
    ) {
      return false;
    }
    if (first == 0xF0u && second < 0x90u) {
      return false;
    }
    if (first == 0xF4u && second > 0x8Fu) {
      return false;
    }

    if (out_scalar != NULL) {
      *out_scalar = (ignis_char_t)(
        ((first & 0x07u) << 18) |
        ((second & 0x3Fu) << 12) |
        ((third & 0x3Fu) << 6) |
        (fourth & 0x3Fu)
      );
    }
    if (out_end != NULL) {
      *out_end = idx + 4;
    }
    return true;
  }

  return false;
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

static i32 ignis_bytes_compare(
  const char *left,
  size_t left_len,
  const char *right,
  size_t right_len
) {
  size_t shared = left_len < right_len ? left_len : right_len;

  for (size_t index = 0; index < shared; index++) {
    unsigned char left_byte = (unsigned char)left[index];
    unsigned char right_byte = (unsigned char)right[index];

    if (left_byte != right_byte) {
      return left_byte < right_byte ? -1 : 1;
    }
  }

  if (left_len == right_len) {
    return 0;
  }

  return left_len < right_len ? -1 : 1;
}

static i64 ignis_bytes_index_of(
  const char *haystack,
  size_t haystack_len,
  const char *needle,
  size_t needle_len
) {
  if (needle_len == 0) {
    return 0;
  }

  if (needle_len > haystack_len) {
    return -1;
  }

  size_t last_start = haystack_len - needle_len;
  for (size_t start = 0; start <= last_start; start++) {
    if (memcmp(haystack + start, needle, needle_len) == 0) {
      return (i64)start;
    }
  }

  return -1;
}

// =============================================================================
// String base API
// =============================================================================

IgnisString ignis_string_new(void) {
  return ignis_string_with_capacity(16);
}

IgnisString ignis_string_with_capacity(size_t cap) {
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

IgnisString ignis_string_from_cstr(const char *cstr) {
  if (cstr == NULL) {
    return ignis_string_new();
  }

  size_t len = strlen(cstr);
  return ignis_string_from_len(cstr, len);
}

IgnisString ignis_string_from_len(const char *s, size_t len) {
  size_t cap = len + 1;
  IgnisString str = ignis_string_with_capacity(cap);

  if (s != NULL && len > 0) {
    memcpy(str.data, s, len);
  }

  str.data[len] = '\0';
  str.len = len;

  return str;
}

IgnisString ignis_string_clone(const IgnisString *s) {
  if (s == NULL || s->data == NULL) {
    return ignis_string_new();
  }

  return ignis_string_from_len(s->data, s->len);
}

void ignis_string_push_char(IgnisString *s, ignis_char_t c) {
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

void ignis_string_push_byte(IgnisString *s, u8 c) {
  if (s == NULL) {
    return;
  }

  size_t required = s->len + 2;
  if (required > s->cap) {
    ignis_string_grow(s, required);
  }

  s->data[s->len] = (char)c;
  s->len++;
  s->data[s->len] = '\0';
}

void ignis_string_push_cstr(IgnisString *s, const char *cstr) {
  if (s == NULL || cstr == NULL) {
    return;
  }

  size_t cstr_len = strlen(cstr);
  if (cstr_len == 0) {
    return;
  }

  size_t required = s->len + cstr_len + 1;
  if (required > s->cap) {
    ignis_string_grow(s, required);
  }

  memcpy(s->data + s->len, cstr, cstr_len);
  s->len += cstr_len;
  s->data[s->len] = '\0';
}

void ignis_string_push_str(IgnisString *s, const IgnisString *other) {
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

const char *ignis_string_cstr(const IgnisString *s) {
  if (s == NULL || s->data == NULL) {
    return "";
  }

  return s->data;
}

size_t ignis_string_len(const IgnisString *s) {
  if (s == NULL) {
    return 0;
  }

  return s->len;
}

size_t ignis_string_cap(const IgnisString *s) {
  if (s == NULL) {
    return 0;
  }

  return s->cap;
}

ignis_char_t ignis_string_char_at(const IgnisString *s, size_t idx, size_t *out_end) {
  ignis_char_t scalar = 0;
  if (!ignis_utf8_decode_at(s, idx, &scalar, out_end)) {
    return 0;
  }

  return scalar;
}

u8 ignis_string_byte_at(const IgnisString *s, size_t idx) {
  if (s == NULL || idx >= s->len) {
    return 0;
  }

  return (u8)s->data[idx];
}

void ignis_string_clear(IgnisString *s) {
  if (s == NULL) {
    return;
  }

  s->len = 0;
  if (s->data != NULL) {
    s->data[0] = '\0';
  }
}

void ignis_string_reserve(IgnisString *s, size_t additional) {
  if (s == NULL) {
    return;
  }

  size_t required = s->len + additional + 1;
  if (required > s->cap) {
    ignis_string_grow(s, required);
  }
}

void ignis_string_drop(IgnisString *s) {
  if (s == NULL) {
    return;
  }

  if (s->data != NULL) {
    ignis_free(s->data);
  }

  s->data = NULL;
  s->len = 0;
  s->cap = 0;
}

// =============================================================================
// String operations
// =============================================================================

i32 ignis_string_compare(const IgnisString *a, const IgnisString *b) {
  const char *a_bytes = (a != NULL && a->data != NULL) ? a->data : "";
  const char *b_bytes = (b != NULL && b->data != NULL) ? b->data : "";
  size_t a_len = (a != NULL) ? a->len : 0;
  size_t b_len = (b != NULL) ? b->len : 0;

  return ignis_bytes_compare(a_bytes, a_len, b_bytes, b_len);
}

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

i64 ignis_string_index_of(const IgnisString *haystack, const IgnisString *needle) {
  if (haystack == NULL || needle == NULL) {
    return -1;
  }

  const char *haystack_bytes = (haystack->data != NULL) ? haystack->data : "";
  const char *needle_bytes = (needle->data != NULL) ? needle->data : "";

  return ignis_bytes_index_of(haystack_bytes, haystack->len, needle_bytes, needle->len);
}

boolean ignis_string_contains(const IgnisString *haystack, const IgnisString *needle) {
  return ignis_string_index_of(haystack, needle) >= 0 ? TRUE : FALSE;
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

void ignis_string_init_new(IgnisString *out) {
  *out = ignis_string_new();
}

void ignis_string_init_with_capacity(IgnisString *out, size_t cap) {
  *out = ignis_string_with_capacity(cap);
}

void ignis_string_init_from_cstr(IgnisString *out, const char *cstr) {
  *out = ignis_string_from_cstr(cstr);
}

void ignis_string_init_from_len(IgnisString *out, const char *s, size_t len) {
  *out = ignis_string_from_len(s, len);
}

void ignis_string_init_clone(IgnisString *out, const IgnisString *s) {
  *out = ignis_string_clone(s);
}

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
