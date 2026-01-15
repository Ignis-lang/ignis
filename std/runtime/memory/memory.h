#pragma once

#include "../ignis_rt.h"

/**
 * Memory allocation and buffer helpers for std/memory.
 */

/**
 * Allocates `size` bytes with the runtime allocator.
 *
 * @param size Size in bytes.
 * @return Pointer to allocated memory (NULL on failure).
 */
void *memoryAllocate(u32 size);
/**
 * Frees a memory block allocated by the runtime allocator.
 *
 * @param ptr Pointer returned by allocation functions.
 */
void memoryDeallocate(void *ptr);
/**
 * Resizes a memory block allocated by the runtime allocator.
 *
 * @param ptr Pointer returned by allocation functions.
 * @param size New size in bytes.
 * @return Pointer to resized memory (NULL on failure).
 */
void *memoryReallocate(void *ptr, u32 size);
/**
 * Allocates `count` elements of `size` bytes each, zero-initialized.
 *
 * @param size Element size in bytes.
 * @param count Number of elements.
 * @return Pointer to allocated memory (NULL on failure).
 */
void *memoryAllocateZeroed(u32 size, u32 count);

/**
 * Creates a new buffer for elements of the given size and type ID.
 *
 * @param elemSize Size of each element in bytes.
 * @param elemTypeId Runtime type identifier for the element type.
 * @return Newly allocated buffer (NULL on failure).
 */
IgnisBuffer *bufNew(u64 elemSize, u32 elemTypeId);
/**
 * Creates a new buffer with reserved capacity.
 *
 * @param elemSize Size of each element in bytes.
 * @param elemTypeId Runtime type identifier for the element type.
 * @param cap Initial capacity in elements.
 * @return Newly allocated buffer (NULL on failure).
 */
IgnisBuffer *bufWithCapacity(u64 elemSize, u32 elemTypeId, u64 cap);
/**
 * Appends a single element to the buffer.
 *
 * @param buf Target buffer.
 * @param elem Pointer to the element to copy.
 */
void bufPush(IgnisBuffer *buf, const void *elem);
/**
 * Returns a mutable pointer to the element at `idx`.
 *
 * @param buf Target buffer.
 * @param idx Zero-based index.
 * @return Pointer to the element or NULL if out of range.
 */
void *bufAt(IgnisBuffer *buf, u64 idx);
/**
 * Returns the current number of elements in the buffer.
 *
 * @param buf Target buffer.
 * @return Element count.
 */
u64 bufLen(const IgnisBuffer *buf);
/**
 * Returns the buffer capacity in elements.
 *
 * @param buf Target buffer.
 * @return Capacity in elements.
 */
u64 bufCap(const IgnisBuffer *buf);
/**
 * Resizes the buffer length to `newLen`.
 *
 * @param buf Target buffer.
 * @param newLen New length in elements.
 */
void bufResize(IgnisBuffer *buf, u64 newLen);
/**
 * Ensures capacity for at least `additional` more elements.
 *
 * @param buf Target buffer.
 * @param additional Additional element capacity.
 */
void bufReserve(IgnisBuffer *buf, u64 additional);
/**
 * Clears the buffer length to zero without freeing capacity.
 *
 * @param buf Target buffer.
 */
void bufClear(IgnisBuffer *buf);
/**
 * Releases the buffer and its storage.
 *
 * @param buf Buffer to drop.
 */
void bufDrop(IgnisBuffer *buf);
