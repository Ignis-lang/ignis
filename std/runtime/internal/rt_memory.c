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


