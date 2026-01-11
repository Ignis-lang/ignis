#pragma once

#include "../ignis_rt.h"

// =============================================================================
// Memory allocation wrappers
// =============================================================================
// These functions provide the interface expected by std/memory/mod.ign.
// They delegate to the core ignis_alloc/ignis_free functions.

void* memoryAllocate(u32 size);
void  memoryDeallocate(void* ptr);
void* memoryReallocate(void* ptr, u32 size);
void* memoryAllocateZeroed(u32 size, u32 count);

// =============================================================================
// Dynamic buffer wrappers
// =============================================================================
// These functions provide the interface expected by std/memory/mod.ign
// for dynamic array operations. They delegate to ignis_buf_* functions.

IgnisBuffer* bufNew(u64 elemSize, u32 elemTypeId);
IgnisBuffer* bufWithCapacity(u64 elemSize, u32 elemTypeId, u64 cap);
void         bufPush(IgnisBuffer* buf, const void* elem);
void*        bufAt(IgnisBuffer* buf, u64 idx);
u64          bufLen(const IgnisBuffer* buf);
u64          bufCap(const IgnisBuffer* buf);
void         bufResize(IgnisBuffer* buf, u64 newLen);
void         bufReserve(IgnisBuffer* buf, u64 additional);
void         bufClear(IgnisBuffer* buf);
void         bufDrop(IgnisBuffer* buf);
