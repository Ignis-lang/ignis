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
 * Header for heap-managed runtime objects.
 * `refcnt` is reserved for future reference counting.
 */
typedef struct {
  IgnisTypeId type_id;
  u32 refcnt;
} IgnisHeader;

/**
 * Heap-managed string type.
 *
 * Invariant: data[len] == '\0'
 * Invariant: cap >= len + 1
 */
typedef struct IgnisString {
  IgnisHeader hdr;
  char *data;
  size_t len;
  size_t cap;
} IgnisString;

/**
 * `string` is an alias for a heap-managed IgnisString pointer.
 */
typedef IgnisString *string;

/**
 * Null pointer alias.
 */
typedef void *null;

// =============================================================================
// Memory allocation
// =============================================================================

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
 * Creates a new empty string.
 */
IgnisString *ignis_string_new(void);

/**
 * Creates a new empty string with at least `cap` capacity.
 */
IgnisString *ignis_string_with_capacity(size_t cap);

/**
 * Creates a new string from a C string.
 */
IgnisString *ignis_string_from_cstr(const char *s);

/**
 * Creates a new string from a byte slice.
 */
IgnisString *ignis_string_from_len(const char *s, size_t len);

/**
 * Creates a clone of an existing string.
 */
IgnisString *ignis_string_clone(const IgnisString *s);

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
 * Returns a null-terminated view of the string.
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
 * Clears the string to length 0.
 */
void ignis_string_clear(IgnisString *s);

/**
 * Ensures capacity for `additional` bytes beyond current length.
 */
void ignis_string_reserve(IgnisString *s, size_t additional);

/**
 * Releases the string and its storage.
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
IgnisString *ignis_string_concat(const IgnisString *a, const IgnisString *b);

/**
 * Creates a substring from `start` with length `len`.
 */
IgnisString *ignis_string_substring(const IgnisString *s, i64 start, i64 len);

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
IgnisString *ignis_string_to_upper(const IgnisString *s);

/**
 * Returns a new string with all characters converted to lowercase.
 */
IgnisString *ignis_string_to_lower(const IgnisString *s);

// =============================================================================
// Number to string conversions
// =============================================================================

IgnisString *ignis_i8_to_string(i8 value);
IgnisString *ignis_i16_to_string(i16 value);
IgnisString *ignis_i32_to_string(i32 value);
IgnisString *ignis_i64_to_string(i64 value);

IgnisString *ignis_u8_to_string(u8 value);
IgnisString *ignis_u16_to_string(u16 value);
IgnisString *ignis_u32_to_string(u32 value);
IgnisString *ignis_u64_to_string(u64 value);

IgnisString *ignis_f32_to_string(f32 value);
IgnisString *ignis_f64_to_string(f64 value);

// =============================================================================
// I/O
// =============================================================================

/**
 * Writes the string to stdout.
 */
void ignis_print(const IgnisString *s);

/**
 * Writes the string to stderr.
 */
void ignis_eprint(const IgnisString *s);


