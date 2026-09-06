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
 * len is measured in bytes. Owned storage preserves interior NUL bytes, so
 * callers that need the full contents must honor `len` instead of stopping at
 * the first interior NUL byte.
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

// Output-pointer variants. These write through a pointer because the caller's
// struct may carry extra trailing fields (the compiler-generated
// __ignis_drop_state) that return-by-value would not account for.
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
