#include "ignis_rt.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// =============================================================================
// Alloc/Free base
// =============================================================================

void *ignis_alloc(size_t size) { return malloc(size); }

void *ignis_realloc(void *ptr, size_t size) { return realloc(ptr, size); }

void *ignis_calloc(size_t count, size_t size) { return calloc(count, size); }

void ignis_free(void *ptr) { free(ptr); }

// =============================================================================
// String - internal helpers
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
// String API
// =============================================================================

IgnisString *ignis_string_new(void) { return ignis_string_with_capacity(16); }

IgnisString *ignis_string_with_capacity(size_t cap) {
  IgnisString *s = (IgnisString *)ignis_alloc(sizeof(IgnisString));
  if (s == NULL) {
    return NULL;
  }

  // Ensure capacity for at least null terminator
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
  // Capacity: len + 1 for null terminator
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

  // Need space for: existing chars + new char + null terminator
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
// Buffer - internal helpers
// =============================================================================

static void ignis_buf_grow(IgnisBuffer *buf, size_t min_cap) {
  if (buf == NULL || buf->elem_size == 0) {
    return;
  }

  size_t new_cap = buf->cap;
  if (new_cap == 0) {
    new_cap = 8;
  }

  while (new_cap < min_cap) {
    new_cap *= 2;
  }

  void *new_data = ignis_realloc(buf->data, new_cap * buf->elem_size);
  if (new_data == NULL) {
    return;
  }

  buf->data = new_data;
  buf->cap = new_cap;
}

// =============================================================================
// Buffer API
// =============================================================================

IgnisBuffer *ignis_buf_new(size_t elem_size, IgnisTypeId elem_type_id) {
  return ignis_buf_with_capacity(elem_size, elem_type_id, 8);
}

IgnisBuffer *ignis_buf_with_capacity(size_t elem_size, IgnisTypeId elem_type_id,
                                     size_t cap) {
  if (elem_size == 0) {
    return NULL;
  }

  IgnisBuffer *buf = (IgnisBuffer *)ignis_alloc(sizeof(IgnisBuffer));
  if (buf == NULL) {
    return NULL;
  }

  buf->hdr.type_id = IGNIS_TYPE_BUFFER_ID;
  buf->hdr.refcnt = 1;

  buf->elem_size = elem_size;
  buf->elem_type_id = elem_type_id;
  buf->len = 0;
  buf->cap = cap;

  if (cap > 0) {
    buf->data = ignis_alloc(cap * elem_size);
    if (buf->data == NULL) {
      ignis_free(buf);
      return NULL;
    }
  } else {
    buf->data = NULL;
  }

  return buf;
}

void ignis_buf_push(IgnisBuffer *buf, const void *elem) {
  if (buf == NULL || elem == NULL) {
    return;
  }

  if (buf->len >= buf->cap) {
    ignis_buf_grow(buf, buf->len + 1);
    if (buf->len >= buf->cap) {
      return;
    }
  }

  void *dest = (char *)buf->data + (buf->len * buf->elem_size);
  memcpy(dest, elem, buf->elem_size);
  buf->len++;
}

void *ignis_buf_at(IgnisBuffer *buf, size_t idx) {
  if (buf == NULL || idx >= buf->len) {
    return NULL;
  }
  return (char *)buf->data + (idx * buf->elem_size);
}

const void *ignis_buf_at_const(const IgnisBuffer *buf, size_t idx) {
  if (buf == NULL || idx >= buf->len) {
    return NULL;
  }
  return (const char *)buf->data + (idx * buf->elem_size);
}

size_t ignis_buf_len(const IgnisBuffer *buf) {
  if (buf == NULL) {
    return 0;
  }
  return buf->len;
}

size_t ignis_buf_cap(const IgnisBuffer *buf) {
  if (buf == NULL) {
    return 0;
  }
  return buf->cap;
}

void ignis_buf_resize(IgnisBuffer *buf, size_t new_len) {
  if (buf == NULL) {
    return;
  }

  if (new_len > buf->cap) {
    ignis_buf_grow(buf, new_len);
    if (buf->cap < new_len) {
      return;
    }
  }

  // If growing, zero-initialize new elements
  if (new_len > buf->len) {
    void *start = (char *)buf->data + (buf->len * buf->elem_size);
    size_t bytes = (new_len - buf->len) * buf->elem_size;
    memset(start, 0, bytes);
  }

  buf->len = new_len;
}

void ignis_buf_reserve(IgnisBuffer *buf, size_t additional) {
  if (buf == NULL) {
    return;
  }

  size_t required = buf->len + additional;
  if (required > buf->cap) {
    ignis_buf_grow(buf, required);
  }
}

void ignis_buf_clear(IgnisBuffer *buf) {
  if (buf == NULL) {
    return;
  }
  buf->len = 0;
}

void ignis_buf_drop(IgnisBuffer *buf) {
  if (buf == NULL) {
    return;
  }

  if (buf->data != NULL) {
    ignis_free(buf->data);
    buf->data = NULL;
  }

  buf->len = 0;
  buf->cap = 0;

  ignis_free(buf);
}
