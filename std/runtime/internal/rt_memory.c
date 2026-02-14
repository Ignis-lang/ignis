/**
 * Ignis runtime: memory allocation and buffer management.
 *
 * Every allocation goes through a hidden AllocHeader that stores the
 * requested size. This lets us maintain live-allocation statistics
 * (bytes and count) without an external data structure. The header
 * is placed immediately before the pointer returned to the caller.
 */

#include "../ignis_rt.h"
#include "rt_internal.h"

// =============================================================================
// Allocation tracking
// =============================================================================

typedef struct {
  _Alignas(max_align_t) size_t size;
} AllocHeader;

static size_t stats_allocs_live = 0;
static size_t stats_bytes_live  = 0;
static size_t stats_alloc_total = 0;
static size_t stats_free_total  = 0;

static AllocHeader *header_from_ptr(void *ptr) {
  return (AllocHeader *)((char *)ptr - sizeof(AllocHeader));
}

static void track_alloc(size_t size) {
  stats_allocs_live += 1;
  stats_bytes_live  += size;
  stats_alloc_total += 1;
}

static void track_free(size_t size) {
  stats_allocs_live -= 1;
  stats_bytes_live  -= size;
  stats_free_total  += 1;
}

IgnisMemStats ignis_mem_stats(void) {
  IgnisMemStats s;
  s.allocs_live = stats_allocs_live;
  s.bytes_live  = stats_bytes_live;
  s.alloc_total = stats_alloc_total;
  s.free_total  = stats_free_total;
  return s;
}

void ignis_mem_reset_stats(void) {
  stats_allocs_live = 0;
  stats_bytes_live  = 0;
  stats_alloc_total = 0;
  stats_free_total  = 0;
}

// =============================================================================
// Allocator API
// =============================================================================

void *ignis_alloc(size_t size) {
  AllocHeader *hdr = (AllocHeader *)malloc(sizeof(AllocHeader) + size);
  if (IGNIS_UNLIKELY(hdr == NULL)) {
    fprintf(stderr, "ignis: out of memory (alloc %zu bytes)\n", size);
    abort();
  }

  hdr->size = size;
  track_alloc(size);
  return (char *)hdr + sizeof(AllocHeader);
}

void *ignis_realloc(void *ptr, size_t size) {
  if (ptr == NULL) {
    return ignis_alloc(size);
  }

  AllocHeader *old_hdr = header_from_ptr(ptr);
  size_t old_size = old_hdr->size;

  AllocHeader *new_hdr = (AllocHeader *)realloc(old_hdr, sizeof(AllocHeader) + size);
  if (IGNIS_UNLIKELY(new_hdr == NULL)) {
    fprintf(stderr, "ignis: out of memory (realloc %zu bytes)\n", size);
    abort();
  }

  stats_bytes_live = stats_bytes_live - old_size + size;
  new_hdr->size = size;
  return (char *)new_hdr + sizeof(AllocHeader);
}

void *ignis_calloc(size_t count, size_t size) {
  size_t total = ignis_checked_mul(count, size);
  if (IGNIS_UNLIKELY(total == 0 && count != 0 && size != 0)) {
    return NULL;
  }

  void *ptr = ignis_alloc(total);
  if (ptr != NULL) {
    memset(ptr, 0, total);
  }
  return ptr;
}

void ignis_free(void *ptr) {
  if (ptr == NULL) {
    return;
  }

  AllocHeader *hdr = header_from_ptr(ptr);
  track_free(hdr->size);
  free(hdr);
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
