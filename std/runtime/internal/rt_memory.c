/**
 * Ignis runtime: memory allocation and buffer management.
 */

#include "../ignis_rt.h"
#include "rt_internal.h"

// =============================================================================
// Allocator API
// =============================================================================

void *ignis_alloc(size_t size) {
  return malloc(size);
}

void *ignis_realloc(void *ptr, size_t size) {
  return realloc(ptr, size);
}

void *ignis_calloc(size_t count, size_t size) {
  return calloc(count, size);
}

void ignis_free(void *ptr) {
  free(ptr);
}

// =============================================================================
// Memory operations
// =============================================================================

void ignis_memcpy(void *dest, const void *src, size_t n) {
  if (dest != NULL && src != NULL && n > 0) {
    memcpy(dest, src, n);
  }
}

void ignis_memmove(void *dest, const void *src, size_t n) {
  if (dest != NULL && src != NULL && n > 0) {
    memmove(dest, src, n);
  }
}

// =============================================================================
// Buffer API (DEPRECATED: remove after migration to Vector<T>)
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

  void *new_data = ignis_realloc(buf->data, ignis_checked_mul(new_cap, buf->elem_size));
  if (new_data == NULL) {
    return;
  }

  buf->data = new_data;
  buf->cap = new_cap;
}

IgnisBuffer *ignis_buf_new(size_t elem_size, IgnisTypeId elem_type_id) {
  return ignis_buf_with_capacity(elem_size, elem_type_id, 8);
}

IgnisBuffer *ignis_buf_with_capacity(size_t elem_size, IgnisTypeId elem_type_id, size_t cap) {
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
    buf->data = ignis_alloc(ignis_checked_mul(cap, elem_size));
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
