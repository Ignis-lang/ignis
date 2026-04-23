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
  void *base;
} AllocHeader;

typedef struct IgnisArenaBlock {
  void *data;
  size_t capacity;
  size_t used;
  size_t alignment;
  struct IgnisArenaBlock *next;
} IgnisArenaBlock;

struct IgnisArena {
  size_t block_size;
  IgnisArenaBlock *head;
  IgnisArenaBlock *current;
};

static size_t stats_allocs_live = 0;
static size_t stats_bytes_live  = 0;
static size_t stats_alloc_total = 0;
static size_t stats_free_total  = 0;

static AllocHeader *header_from_ptr(void *ptr) {
  return (AllocHeader *)((char *)ptr - sizeof(AllocHeader));
}

static bool is_power_of_two(size_t value) {
  return value != 0 && (value & (value - 1)) == 0;
}

static void ignis_abort_invalid_alignment(size_t alignment) {
  fprintf(stderr, "ignis: invalid alignment (%zu)\n", alignment);
  abort();
}

static void ignis_abort_oom(
  const char *operation,
  size_t size
) {
  fprintf(stderr, "ignis: out of memory (%s %zu bytes)\n", operation, size);
  abort();
}

static void ignis_abort_overflow(
  size_t count,
  size_t size
) {
  fprintf(stderr, "ignis: allocation size overflow (%zu * %zu)\n", count, size);
  abort();
}

static void *alloc_aligned_raw(
  size_t size,
  size_t alignment
) {
  if (size == 0) {
    return NULL;
  }

  if (!is_power_of_two(alignment)) {
    ignis_abort_invalid_alignment(alignment);
  }

  size_t min_alignment = _Alignof(max_align_t);
  if (alignment < min_alignment) {
    alignment = min_alignment;
  }

  size_t total = sizeof(AllocHeader) + size + alignment - 1;
  void *base = malloc(total);
  if (IGNIS_UNLIKELY(base == NULL)) {
    ignis_abort_oom("alloc", size);
  }

  uintptr_t start = (uintptr_t)base + sizeof(AllocHeader);
  uintptr_t aligned = ignis_align_up(start, alignment);
  AllocHeader *hdr = (AllocHeader *)(aligned - sizeof(AllocHeader));
  hdr->size = size;
  hdr->base = base;
  return (void *)aligned;
}

static IgnisArenaBlock *ignis_arena_block_create(
  size_t capacity,
  size_t alignment
) {
  IgnisArenaBlock *block = (IgnisArenaBlock *)ignis_alloc(sizeof(IgnisArenaBlock));
  block->data = ignis_alloc_aligned(capacity, alignment);
  block->capacity = capacity;
  block->used = 0;
  block->alignment = alignment < _Alignof(max_align_t) ? _Alignof(max_align_t) : alignment;
  block->next = NULL;
  return block;
}

static void *ignis_arena_block_alloc(
  IgnisArenaBlock *block,
  size_t size,
  size_t alignment
) {
  if (size == 0 || block == NULL || alignment > block->alignment) {
    return NULL;
  }

  uintptr_t start = (uintptr_t)block->data + block->used;
  uintptr_t aligned = ignis_align_up(start, alignment);
  size_t next_used = (size_t)(aligned - (uintptr_t)block->data) + size;
  if (next_used > block->capacity) {
    return NULL;
  }

  block->used = next_used;
  return (void *)aligned;
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
  void *ptr = alloc_aligned_raw(size, _Alignof(max_align_t));
  if (ptr == NULL) {
    return NULL;
  }

  track_alloc(size);
  return ptr;
}

void *ignis_alloc_aligned(size_t size, size_t alignment) {
  void *ptr = alloc_aligned_raw(size, alignment);
  if (ptr == NULL) {
    return NULL;
  }

  track_alloc(size);
  return ptr;
}

void *ignis_realloc(void *ptr, size_t size) {
  return ignis_realloc_aligned(ptr, size, _Alignof(max_align_t));
}

void *ignis_realloc_aligned(void *ptr, size_t size, size_t alignment) {
  if (ptr == NULL) {
    return ignis_alloc_aligned(size, alignment);
  }

  if (size == 0) {
    ignis_free(ptr);
    return NULL;
  }

  AllocHeader *old_hdr = header_from_ptr(ptr);
  size_t old_size = old_hdr->size;

  void *new_ptr = alloc_aligned_raw(size, alignment);
  if (new_ptr == NULL) {
    return NULL;
  }

  size_t copy_size = old_size < size ? old_size : size;
  if (copy_size > 0) {
    memcpy(new_ptr, ptr, copy_size);
  }

  stats_bytes_live = stats_bytes_live - old_size + size;
  free(old_hdr->base);
  return new_ptr;
}

void *ignis_calloc(size_t count, size_t size) {
  return ignis_calloc_aligned(count, size, _Alignof(max_align_t));
}

void *ignis_calloc_aligned(size_t count, size_t size, size_t alignment) {
  size_t total = ignis_checked_mul(count, size);
  if (IGNIS_UNLIKELY(count != 0 && size != 0 && total == 0)) {
    ignis_abort_overflow(count, size);
  }

  void *ptr = ignis_alloc_aligned(total, alignment);
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
  free(hdr->base);
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

IgnisArena *ignis_arena_create(size_t block_size) {
  IgnisArena *arena = (IgnisArena *)ignis_alloc(sizeof(IgnisArena));
  arena->block_size = block_size == 0 ? 1024 : block_size;
  arena->head = NULL;
  arena->current = NULL;
  return arena;
}

void *ignis_arena_allocate(IgnisArena *arena, size_t size, size_t alignment) {
  if (arena == NULL || size == 0) {
    return NULL;
  }

  if (!is_power_of_two(alignment)) {
    ignis_abort_invalid_alignment(alignment);
  }

  IgnisArenaBlock *block = arena->current;
  while (block != NULL) {
    void *ptr = ignis_arena_block_alloc(block, size, alignment);
    if (ptr != NULL) {
      arena->current = block;
      return ptr;
    }

    block = block->next;
  }

  size_t min_capacity = size + alignment - 1;
  size_t growth_base = arena->current != NULL ? arena->current->capacity * 2 : arena->block_size;
  size_t capacity = growth_base > arena->block_size ? growth_base : arena->block_size;
  if (capacity < min_capacity) {
    capacity = min_capacity;
  }

  IgnisArenaBlock *new_block = ignis_arena_block_create(capacity, alignment);
  if (arena->head == NULL) {
    arena->head = new_block;
  } else if (arena->current != NULL) {
    arena->current->next = new_block;
  } else {
    IgnisArenaBlock *tail = arena->head;
    while (tail->next != NULL) {
      tail = tail->next;
    }
    tail->next = new_block;
  }

  arena->current = new_block;
  return ignis_arena_block_alloc(new_block, size, alignment);
}

void ignis_arena_reset(IgnisArena *arena) {
  if (arena == NULL) {
    return;
  }

  IgnisArenaBlock *block = arena->head;
  while (block != NULL) {
    block->used = 0;
    block = block->next;
  }

  arena->current = arena->head;
}

void ignis_arena_destroy(IgnisArena *arena) {
  if (arena == NULL) {
    return;
  }

  IgnisArenaBlock *block = arena->head;
  while (block != NULL) {
    IgnisArenaBlock *next = block->next;
    ignis_free(block->data);
    ignis_free(block);
    block = next;
  }

  ignis_free(arena);
}

u64 ignis_hash_fnv1a_cstr(u64 state, const char *value) {
  if (value == NULL) {
    return state;
  }

  const unsigned char *bytes = (const unsigned char *)value;
  while (*bytes != '\0') {
    state ^= (u64)(*bytes);
    state *= 1099511628211ULL;
    bytes += 1;
  }

  return state;
}
