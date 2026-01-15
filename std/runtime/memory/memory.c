#include "memory.h"

// =============================================================================
// Memory allocation wrappers
// =============================================================================

void *memoryAllocate(u32 size) { return ignis_alloc((size_t)size); }

void memoryDeallocate(void *ptr) { ignis_free(ptr); }

void *memoryReallocate(void *ptr, u32 size) {
  return ignis_realloc(ptr, (size_t)size);
}

void *memoryAllocateZeroed(u32 size, u32 count) {
  return ignis_calloc((size_t)count, (size_t)size);
}

// =============================================================================
// Dynamic buffer wrappers
// =============================================================================

IgnisBuffer *bufNew(u64 elemSize, u32 elemTypeId) {
  return ignis_buf_new((size_t)elemSize, (IgnisTypeId)elemTypeId);
}

IgnisBuffer *bufWithCapacity(u64 elemSize, u32 elemTypeId, u64 cap) {
  return ignis_buf_with_capacity((size_t)elemSize, (IgnisTypeId)elemTypeId,
                                 (size_t)cap);
}

void bufPush(IgnisBuffer *buf, const void *elem) { ignis_buf_push(buf, elem); }

void *bufAt(IgnisBuffer *buf, u64 idx) {
  return ignis_buf_at(buf, (size_t)idx);
}

u64 bufLen(const IgnisBuffer *buf) { return (u64)ignis_buf_len(buf); }

u64 bufCap(const IgnisBuffer *buf) { return (u64)ignis_buf_cap(buf); }

void bufResize(IgnisBuffer *buf, u64 newLen) {
  ignis_buf_resize(buf, (size_t)newLen);
}

void bufReserve(IgnisBuffer *buf, u64 additional) {
  ignis_buf_reserve(buf, (size_t)additional);
}

void bufClear(IgnisBuffer *buf) { ignis_buf_clear(buf); }

void bufDrop(IgnisBuffer *buf) { ignis_buf_drop(buf); }
