#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include <unistd.h>

// =============================================================================
// Runtime type definitions
//
// Both compilers emit this same block into every translation unit they produce,
// under this guard, so a unit that also includes this header keeps exactly one
// definition of each name. Keep the two copies in step.
// =============================================================================

#ifndef IGNIS_RT_TYPES_H
#define IGNIS_RT_TYPES_H

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
typedef u32 ignis_char_t;

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
 * Value-type UTF-8 byte buffer with heap-managed storage.
 *
 * IgnisString is a value type: instances live inline (on the stack or inside
 * other structs). Only the `data` buffer is heap-allocated. Functions that
 * create new strings return IgnisString by value. `String::drop` in
 * `std/string` frees the data buffer and zeroes the struct, but never frees
 * the struct itself.
 *
 * len is measured in bytes. Storage is owned and may contain interior NUL
 * bytes; callers that need the full contents must honor `len` instead of
 * stopping at the first interior NUL byte.
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

#endif // IGNIS_RT_TYPES_H

// =============================================================================
// Runtime entry point
// =============================================================================

/**
 * Initializes the runtime before user code runs. Defined in Ignis
 * (`std/process/runtime`, exported as this C symbol); declared here because
 * the generated `main` wrappers and the test harness include only this header.
 * The signature mirrors the emitted one: `argv` is passed as an opaque pointer.
 */
void ignis_runtime_init(i32 argc, void *argv);

// =============================================================================
// String access API
//
// Construction, mutation and destruction now live in `std/string`, which owns
// the `IgnisString` buffer end to end. What remains here are the read-only and
// derived-string operations that have not moved yet.
// =============================================================================

/**
 * Returns a null-terminated borrowed view of the string data.
 *
 * This view is suitable for C interop, but consumers that rely on C-string
 * semantics may stop at the first interior NUL byte.
 */
const char *ignis_string_cstr(const IgnisString *s);

/**
 * Decodes the scalar that starts at byte `idx`.
 *
 * On success, returns the scalar and writes the exclusive end byte offset to
 * `out_end`. On failure or non-boundary input, returns 0 and leaves `*out_end`
 * equal to `idx`.
 */
ignis_char_t ignis_string_char_at(const IgnisString *s, size_t idx, size_t *out_end);

/**
 * Returns the raw byte at `idx`, or 0 if out of range.
 */
u8 ignis_string_byte_at(const IgnisString *s, size_t idx);

// =============================================================================
// String operations
// =============================================================================

/**
 * Compares two strings lexicographically over all owned bytes.
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
 * Returns the first byte index of `needle` in `haystack`, or -1 if not found.
 *
 * Search is length-aware and does not stop at interior NUL bytes.
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
// Filesystem helpers (rt_fs.c)
// =============================================================================

int ignis_stat_call(
    const char *path,
    u64 *out_dev, u64 *out_ino, u32 *out_mode, u64 *out_nlink,
    u32 *out_uid, u32 *out_gid, i64 *out_size,
    i64 *out_atime, i64 *out_mtime, i64 *out_ctime,
    i64 *out_blksize, i64 *out_blocks);

int ignis_fstat_call(
    int fd,
    u64 *out_dev, u64 *out_ino, u32 *out_mode, u64 *out_nlink,
    u32 *out_uid, u32 *out_gid, i64 *out_size,
    i64 *out_atime, i64 *out_mtime, i64 *out_ctime,
    i64 *out_blksize, i64 *out_blocks);

int ignis_lstat_call(
    const char *path,
    u64 *out_dev, u64 *out_ino, u32 *out_mode, u64 *out_nlink,
    u32 *out_uid, u32 *out_gid, i64 *out_size,
    i64 *out_atime, i64 *out_mtime, i64 *out_ctime,
    i64 *out_blksize, i64 *out_blocks);

int ignis_open3(const char *pathname, int flags, u32 mode);

const char *ignis_dirent_name(void *entry);
u64 ignis_dirent_ino(void *entry);
u8 ignis_dirent_type(void *entry);
int ignis_readdir_call(void *dirp, u64 *out_name, u64 *out_ino, u8 *out_type);
int ignis_remove_dir_all_call(const char *path);
