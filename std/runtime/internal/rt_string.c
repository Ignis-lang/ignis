/**
 * Ignis runtime: string management and conversions.
 */

#include "../ignis_rt.h"
#include "rt_internal.h"

#define INT_BUF_SIZE 24
#define FLOAT_BUF_SIZE 32

// =============================================================================
// Internal helpers
// =============================================================================

static void ignis_string_grow(IgnisString *s, size_t min_cap) {
  if (s == NULL) {
    return;
  }

  size_t new_cap = s->cap;
  if (new_cap == 0) {
    new_cap = 16;
  }

  while (new_cap < min_cap) {
    new_cap *= 2;
  }

  char *new_data = (char *)ignis_realloc(s->data, new_cap);
  if (new_data == NULL) {
    return;
  }

  s->data = new_data;
  s->cap = new_cap;
}

// =============================================================================
// String base API
// =============================================================================

IgnisString *ignis_string_new(void) {
  return ignis_string_with_capacity(16);
}

IgnisString *ignis_string_with_capacity(size_t cap) {
  IgnisString *s = (IgnisString *)ignis_alloc(sizeof(IgnisString));
  if (s == NULL) {
    return NULL;
  }

  if (cap < 1) {
    cap = 1;
  }

  s->hdr.type_id = IGNIS_TYPE_STRING_ID;
  s->hdr.refcnt = 1;

  s->data = (char *)ignis_alloc(cap);
  if (s->data == NULL) {
    ignis_free(s);
    return NULL;
  }

  s->data[0] = '\0';
  s->len = 0;
  s->cap = cap;

  return s;
}

IgnisString *ignis_string_from_cstr(const char *cstr) {
  if (cstr == NULL) {
    return ignis_string_new();
  }

  size_t len = strlen(cstr);
  return ignis_string_from_len(cstr, len);
}

IgnisString *ignis_string_from_len(const char *s, size_t len) {
  size_t cap = len + 1;
  IgnisString *str = ignis_string_with_capacity(cap);
  if (str == NULL) {
    return NULL;
  }

  if (s != NULL && len > 0) {
    memcpy(str->data, s, len);
  }

  str->data[len] = '\0';
  str->len = len;

  return str;
}

IgnisString *ignis_string_clone(const IgnisString *s) {
  if (s == NULL) {
    return ignis_string_new();
  }

  return ignis_string_from_len(s->data, s->len);
}

void ignis_string_push_char(IgnisString *s, char c) {
  if (s == NULL) {
    return;
  }

  size_t required = s->len + 2;
  if (required > s->cap) {
    ignis_string_grow(s, required);
    if (s->cap < required) {
      return;
    }
  }

  s->data[s->len] = c;
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
    if (s->cap < required) {
      return;
    }
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
    if (s->cap < required) {
      return;
    }
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

char ignis_string_char_at(const IgnisString *s, size_t idx) {
  if (s == NULL || idx >= s->len) {
    return '\0';
  }

  return s->data[idx];
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
    s->data = NULL;
  }

  s->len = 0;
  s->cap = 0;

  ignis_free(s);
}

// =============================================================================
// String operations
// =============================================================================

i32 ignis_string_compare(const IgnisString *a, const IgnisString *b) {
  const char *a_cstr = (a != NULL) ? ignis_string_cstr(a) : "";
  const char *b_cstr = (b != NULL) ? ignis_string_cstr(b) : "";

  return (i32)strcmp(a_cstr, b_cstr);
}

IgnisString *ignis_string_concat(const IgnisString *a, const IgnisString *b) {
  IgnisString *result = ignis_string_new();
  if (result == NULL) {
    return NULL;
  }

  if (a != NULL) {
    ignis_string_push_str(result, a);
  }

  if (b != NULL) {
    ignis_string_push_str(result, b);
  }

  return result;
}

IgnisString *ignis_string_substring(const IgnisString *s, i64 start, i64 len) {
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

  const char *h_cstr = ignis_string_cstr(haystack);
  const char *n_cstr = ignis_string_cstr(needle);

  const char *pos = strstr(h_cstr, n_cstr);
  if (pos == NULL) {
    return -1;
  }

  return (i64)(pos - h_cstr);
}

boolean ignis_string_contains(const IgnisString *haystack, const IgnisString *needle) {
  return ignis_string_index_of(haystack, needle) >= 0 ? TRUE : FALSE;
}

IgnisString *ignis_string_to_upper(const IgnisString *s) {
  if (s == NULL) {
    return ignis_string_new();
  }

  size_t len = ignis_string_len(s);
  const char *data = ignis_string_cstr(s);

  IgnisString *result = ignis_string_with_capacity(len + 1);
  if (result == NULL) {
    return NULL;
  }

  for (size_t i = 0; i < len; i++) {
    ignis_string_push_char(result, (char)toupper((unsigned char)data[i]));
  }

  return result;
}

IgnisString *ignis_string_to_lower(const IgnisString *s) {
  if (s == NULL) {
    return ignis_string_new();
  }

  size_t len = ignis_string_len(s);
  const char *data = ignis_string_cstr(s);

  IgnisString *result = ignis_string_with_capacity(len + 1);
  if (result == NULL) {
    return NULL;
  }

  for (size_t i = 0; i < len; i++) {
    ignis_string_push_char(result, (char)tolower((unsigned char)data[i]));
  }

  return result;
}

// =============================================================================
// Number to string conversions
// =============================================================================

IgnisString *ignis_i8_to_string(i8 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%d", (int)value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString *ignis_i16_to_string(i16 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%d", (int)value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString *ignis_i32_to_string(i32 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%d", value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString *ignis_i64_to_string(i64 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%ld", value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString *ignis_u8_to_string(u8 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%u", (unsigned int)value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString *ignis_u16_to_string(u16 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%u", (unsigned int)value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString *ignis_u32_to_string(u32 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%u", value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString *ignis_u64_to_string(u64 value) {
  char buf[INT_BUF_SIZE];
  int len = snprintf(buf, INT_BUF_SIZE, "%lu", value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString *ignis_f32_to_string(f32 value) {
  char buf[FLOAT_BUF_SIZE];
  int len = snprintf(buf, FLOAT_BUF_SIZE, "%g", (double)value);

  return ignis_string_from_len(buf, (size_t)len);
}

IgnisString *ignis_f64_to_string(f64 value) {
  char buf[FLOAT_BUF_SIZE];
  int len = snprintf(buf, FLOAT_BUF_SIZE, "%g", value);

  return ignis_string_from_len(buf, (size_t)len);
}
