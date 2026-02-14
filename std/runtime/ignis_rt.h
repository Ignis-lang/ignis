#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <unistd.h>

// =============================================================================
// Primitive type aliases
// =============================================================================

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef float f32;
typedef double f64;

typedef u8 boolean;

typedef u32 ignis_atom_t;

typedef void *Pointer;

#define TRUE 1
#define FALSE 0

// =============================================================================
// Runtime type identifiers
// =============================================================================

typedef u32 IgnisTypeId;

#define IGNIS_TYPE_I8_ID 0
#define IGNIS_TYPE_I16_ID 1
#define IGNIS_TYPE_I32_ID 2
#define IGNIS_TYPE_I64_ID 3
#define IGNIS_TYPE_U8_ID 4
#define IGNIS_TYPE_U16_ID 5
#define IGNIS_TYPE_U32_ID 6
#define IGNIS_TYPE_U64_ID 7
#define IGNIS_TYPE_F32_ID 8
#define IGNIS_TYPE_F64_ID 9
#define IGNIS_TYPE_BOOL_ID 10
#define IGNIS_TYPE_CHAR_ID 11
#define IGNIS_TYPE_STRING_ID 12
#define IGNIS_TYPE_PTR_ID 200

// =============================================================================
// Runtime structures
// =============================================================================

/**
 * Value-type string with heap-managed character buffer.
 *
 * IgnisString is a value type: instances live inline (on the stack or inside
 * other structs).  Only the `data` buffer is heap-allocated.  Functions that
 * create new strings return IgnisString by value.  `ignis_string_drop` frees
 * the data buffer and zeroes the struct, but does NOT free the struct itself.
 *
 * Invariant: data[len] == '\0'
 * Invariant: cap >= len + 1
 */
typedef struct IgnisString {
  char *data;
  size_t len;
  size_t cap;
} IgnisString;

/**
 * Null pointer alias.
 */
typedef void *null;

// =============================================================================
// Reference counting (Rc)
// =============================================================================

/**
 * Function pointer type for drop callbacks.
 * Receives a pointer to the payload (not the RcBox header).
 */
typedef void (*IgnisDropFn)(void *);

/**
 * Header for reference-counted allocations.
 *
 * Layout in memory: [IgnisRcBox header][payload bytes...]
 * The payload starts at (char*)rc + sizeof(IgnisRcBox), aligned to max_align_t.
 *
 * The allocation is freed when both refcount and weak_count reach 0.
 */
typedef struct {
  uint32_t refcount;
  uint32_t weak_count;
  IgnisDropFn drop_fn;
  size_t payload_size;
  size_t payload_align;
} IgnisRcBox;

/**
 * Allocates an IgnisRcBox with `payload_size` bytes of payload.
 * The refcount is initialized to 1.
 *
 * @param payload_size Size of the payload in bytes.
 * @param payload_align Required payload alignment in bytes (power-of-two).
 * @param drop_fn Optional destructor called on the payload when refcount reaches 0.
 *                Pass NULL if no cleanup is needed.
 * @return Pointer to the IgnisRcBox, or NULL on allocation failure.
 */
IgnisRcBox *ignis_rc_alloc(size_t payload_size, size_t payload_align, IgnisDropFn drop_fn);

/**
 * Increments the reference count.
 *
 * @param rc Pointer to an IgnisRcBox (must not be NULL).
 */
void ignis_rc_retain(IgnisRcBox *rc);

/**
 * Decrements the reference count.
 * When the count reaches 0, calls drop_fn(payload) if set.
 * Frees the allocation only if weak_count is also 0.
 *
 * @param rc Pointer to an IgnisRcBox (must not be NULL).
 */
void ignis_rc_release(IgnisRcBox *rc);

/**
 * Returns a pointer to the payload stored after the IgnisRcBox header.
 *
 * @param rc Pointer to an IgnisRcBox (must not be NULL).
 * @return Pointer to the payload.
 */
void *ignis_rc_get(IgnisRcBox *rc);

/**
 * Returns the current strong reference count.
 */
uint32_t ignis_rc_count(const IgnisRcBox *rc);

/**
 * Increments the weak count (creates a weak reference).
 */
void ignis_rc_downgrade(IgnisRcBox *rc);

/**
 * If refcount > 0, increments it and returns `rc`. Otherwise returns NULL.
 */
IgnisRcBox *ignis_rc_upgrade(IgnisRcBox *rc);

/**
 * Increments the weak count (clones a weak reference).
 */
void ignis_weak_retain(IgnisRcBox *rc);

/**
 * Decrements the weak count.
 * Frees the allocation if both counts reach 0.
 */
void ignis_weak_release(IgnisRcBox *rc);

/**
 * Returns the current weak reference count.
 */
uint32_t ignis_weak_count(const IgnisRcBox *rc);

// =============================================================================
// Memory allocation
// =============================================================================

typedef struct {
  size_t allocs_live;
  size_t bytes_live;
  size_t alloc_total;
  size_t free_total;
} IgnisMemStats;

IgnisMemStats ignis_mem_stats(void);
void ignis_mem_reset_stats(void);

/**
 * Allocates `size` bytes with the runtime allocator.
 */
void *ignis_alloc(size_t size);

/**
 * Resizes a previously allocated block.
 */
void *ignis_realloc(void *ptr, size_t size);

/**
 * Allocates `count` elements of `size` bytes, zero-initialized.
 */
void *ignis_calloc(size_t count, size_t size);

/**
 * Frees a previously allocated block.
 */
void ignis_free(void *ptr);

/**
 * Copies `n` bytes from `src` to `dest`. Regions must not overlap.
 */
void ignis_memcpy(void *dest, const void *src, size_t n);

/**
 * Copies `n` bytes from `src` to `dest`. Handles overlapping regions.
 */
void ignis_memmove(void *dest, const void *src, size_t n);

// =============================================================================
// String base API
// =============================================================================

/**
 * Creates a new empty string with default capacity.
 */
IgnisString ignis_string_new(void);

/**
 * Creates a new empty string with at least `cap` capacity.
 */
IgnisString ignis_string_with_capacity(size_t cap);

/**
 * Creates a new string from a null-terminated C string.
 */
IgnisString ignis_string_from_cstr(const char *s);

/**
 * Creates a new string from a byte slice of the given length.
 */
IgnisString ignis_string_from_len(const char *s, size_t len);

/**
 * Creates a deep copy of an existing string.
 */
IgnisString ignis_string_clone(const IgnisString *s);

/**
 * Appends a character to the string in place.
 */
void ignis_string_push_char(IgnisString *s, char c);

/**
 * Appends a C string to the string in place.
 */
void ignis_string_push_cstr(IgnisString *s, const char *cstr);

/**
 * Appends another IgnisString to the string in place.
 */
void ignis_string_push_str(IgnisString *s, const IgnisString *other);

/**
 * Returns a null-terminated view of the string data.
 */
const char *ignis_string_cstr(const IgnisString *s);

/**
 * Returns the length of the string in bytes.
 */
size_t ignis_string_len(const IgnisString *s);

/**
 * Returns the capacity of the string buffer in bytes.
 */
size_t ignis_string_cap(const IgnisString *s);

/**
 * Returns the character at `idx`, or '\0' if out of range.
 */
char ignis_string_char_at(const IgnisString *s, size_t idx);

/**
 * Clears the string to length 0 without releasing capacity.
 */
void ignis_string_clear(IgnisString *s);

/**
 * Ensures capacity for `additional` bytes beyond current length.
 */
void ignis_string_reserve(IgnisString *s, size_t additional);

/**
 * Releases the data buffer and zeroes the struct.
 * Does NOT free the IgnisString struct itself (it is a value type).
 */
void ignis_string_drop(IgnisString *s);

// =============================================================================
// String operations
// =============================================================================

/**
 * Compares two strings lexicographically.
 *
 * @return Negative if a < b, zero if equal, positive if a > b.
 */
i32 ignis_string_compare(const IgnisString *a, const IgnisString *b);

/**
 * Creates a new string by concatenating `a` and `b`.
 */
IgnisString ignis_string_concat(const IgnisString *a, const IgnisString *b);

/**
 * Creates a substring from `start` with length `len`.
 */
IgnisString ignis_string_substring(const IgnisString *s, i64 start, i64 len);

/**
 * Returns the first index of `needle` in `haystack`, or -1 if not found.
 */
i64 ignis_string_index_of(const IgnisString *haystack, const IgnisString *needle);

/**
 * Returns TRUE if `needle` appears in `haystack`.
 */
boolean ignis_string_contains(const IgnisString *haystack, const IgnisString *needle);

/**
 * Returns a new string with all characters converted to uppercase.
 */
IgnisString ignis_string_to_upper(const IgnisString *s);

/**
 * Returns a new string with all characters converted to lowercase.
 */
IgnisString ignis_string_to_lower(const IgnisString *s);

// =============================================================================
// String init functions (output-pointer variants)
//
// These write into a pre-allocated IgnisString through a pointer, avoiding
// return-by-value ABI issues when the caller's struct has a different size
// (e.g. the compiler-generated struct has an extra __ignis_drop_state field).
// =============================================================================

void ignis_string_init_new(IgnisString *out);
void ignis_string_init_with_capacity(IgnisString *out, size_t cap);
void ignis_string_init_from_cstr(IgnisString *out, const char *cstr);
void ignis_string_init_from_len(IgnisString *out, const char *s, size_t len);
void ignis_string_init_clone(IgnisString *out, const IgnisString *s);
void ignis_string_init_concat(IgnisString *out, const IgnisString *a, const IgnisString *b);
void ignis_string_init_substring(IgnisString *out, const IgnisString *s, i64 start, i64 len);
void ignis_string_init_to_upper(IgnisString *out, const IgnisString *s);
void ignis_string_init_to_lower(IgnisString *out, const IgnisString *s);

// =============================================================================
// Number to string conversions
// =============================================================================

IgnisString ignis_i8_to_string(i8 value);
IgnisString ignis_i16_to_string(i16 value);
IgnisString ignis_i32_to_string(i32 value);
IgnisString ignis_i64_to_string(i64 value);

IgnisString ignis_u8_to_string(u8 value);
IgnisString ignis_u16_to_string(u16 value);
IgnisString ignis_u32_to_string(u32 value);
IgnisString ignis_u64_to_string(u64 value);

IgnisString ignis_f32_to_string(f32 value);
IgnisString ignis_f64_to_string(f64 value);

// Output-pointer variants for number-to-string conversions.
void ignis_string_init_from_i8(IgnisString *out, i8 value);
void ignis_string_init_from_i16(IgnisString *out, i16 value);
void ignis_string_init_from_i32(IgnisString *out, i32 value);
void ignis_string_init_from_i64(IgnisString *out, i64 value);
void ignis_string_init_from_u8(IgnisString *out, u8 value);
void ignis_string_init_from_u16(IgnisString *out, u16 value);
void ignis_string_init_from_u32(IgnisString *out, u32 value);
void ignis_string_init_from_u64(IgnisString *out, u64 value);
void ignis_string_init_from_f32(IgnisString *out, f32 value);
void ignis_string_init_from_f64(IgnisString *out, f64 value);

// =============================================================================
// I/O
// =============================================================================

/**
 * Writes the string to stdout.
 */
void ignis_print(const IgnisString *s);

/**
 * Writes the string to stdout followed by a newline.
 */
void ignis_println(const IgnisString *s);

/**
 * Writes the string to stderr.
 */
void ignis_eprint(const IgnisString *s);

/**
 * Writes the string to stderr followed by a newline.
 */
void ignis_eprintln(const IgnisString *s);
