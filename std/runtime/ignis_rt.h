#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

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

#define TRUE 1
#define FALSE 0

// =============================================================================
// IgnisTypeId
// =============================================================================

typedef u32 IgnisTypeId;

// Primitive type IDs (match types/types.c)
#define IGNIS_TYPE_I8_ID       0
#define IGNIS_TYPE_I16_ID      1
#define IGNIS_TYPE_I32_ID      2
#define IGNIS_TYPE_I64_ID      3
#define IGNIS_TYPE_U8_ID       4
#define IGNIS_TYPE_U16_ID      5
#define IGNIS_TYPE_U32_ID      6
#define IGNIS_TYPE_U64_ID      7
#define IGNIS_TYPE_F32_ID      8
#define IGNIS_TYPE_F64_ID      9
#define IGNIS_TYPE_BOOL_ID     10
#define IGNIS_TYPE_CHAR_ID     11
#define IGNIS_TYPE_STRING_ID   12
#define IGNIS_TYPE_UNKNOWN_ID  13

// Heap object type IDs
#define IGNIS_TYPE_BUFFER_ID   100

// Pointer type ID
#define IGNIS_TYPE_PTR_ID      200

// =============================================================================
// Flags for IgnisUnknown
// =============================================================================

#define IGNIS_UF_NONE   0
#define IGNIS_UF_OWNED  (1 << 0)  // ptr_val is owned, must be dropped
#define IGNIS_UF_HEAP   (1 << 1)  // points to object with IgnisHeader
#define IGNIS_UF_RAW    (1 << 2)  // raw pointer, never drop

// =============================================================================
// IgnisUnknown - tagged union for unknown/any
// =============================================================================

typedef struct {
    IgnisTypeId type_id;
    u32 flags;
    union {
        i64 i64_val;
        u64 u64_val;
        f64 f64_val;
        f32 f32_val;
        void* ptr_val;
    } payload;
} IgnisUnknown;

// =============================================================================
// IgnisHeader - header for heap-managed objects
// =============================================================================

typedef struct {
    IgnisTypeId type_id;
    u32 refcnt;              // reserved for future RC
    void (*drop_fn)(void*);
} IgnisHeader;

// =============================================================================
// IgnisString - heap-managed string
// =============================================================================
// Invariant: data[len] == '\0' always
// Invariant: cap >= len + 1

typedef struct IgnisString {
    IgnisHeader hdr;
    char* data;
    size_t len;
    size_t cap;
} IgnisString;

// Alias for compatibility
typedef IgnisString* string;

// =============================================================================
// IgnisBuffer - generic dynamic array
// =============================================================================

typedef struct {
    IgnisHeader hdr;
    void* data;
    size_t len;
    size_t cap;
    size_t elem_size;
    IgnisTypeId elem_type_id;
} IgnisBuffer;

// =============================================================================
// Null pointer type
// =============================================================================

typedef void* null;

// =============================================================================
// Alloc/Free base
// =============================================================================

void* ignis_alloc(size_t size);
void* ignis_realloc(void* ptr, size_t size);
void* ignis_calloc(size_t count, size_t size);
void  ignis_free(void* ptr);

// =============================================================================
// Drop generic
// =============================================================================

void ignis_drop_unknown(IgnisUnknown v);
void ignis_drop_obj(void* obj);

// =============================================================================
// String API
// =============================================================================

IgnisString* ignis_string_new(void);
IgnisString* ignis_string_with_capacity(size_t cap);
IgnisString* ignis_string_from_cstr(const char* s);
IgnisString* ignis_string_from_len(const char* s, size_t len);
IgnisString* ignis_string_clone(const IgnisString* s);

void         ignis_string_push_char(IgnisString* s, char c);
void         ignis_string_push_cstr(IgnisString* s, const char* cstr);
void         ignis_string_push_str(IgnisString* s, const IgnisString* other);

const char*  ignis_string_cstr(const IgnisString* s);
size_t       ignis_string_len(const IgnisString* s);
size_t       ignis_string_cap(const IgnisString* s);
char         ignis_string_char_at(const IgnisString* s, size_t idx);

void         ignis_string_clear(IgnisString* s);
void         ignis_string_reserve(IgnisString* s, size_t additional);

void         ignis_string_drop(void* obj);

// =============================================================================
// Buffer API
// =============================================================================

IgnisBuffer* ignis_buf_new(size_t elem_size, IgnisTypeId elem_type_id);
IgnisBuffer* ignis_buf_with_capacity(size_t elem_size, IgnisTypeId elem_type_id, size_t cap);

void         ignis_buf_push(IgnisBuffer* buf, const void* elem);
void*        ignis_buf_at(IgnisBuffer* buf, size_t idx);
const void*  ignis_buf_at_const(const IgnisBuffer* buf, size_t idx);
size_t       ignis_buf_len(const IgnisBuffer* buf);
size_t       ignis_buf_cap(const IgnisBuffer* buf);

void         ignis_buf_resize(IgnisBuffer* buf, size_t new_len);
void         ignis_buf_reserve(IgnisBuffer* buf, size_t additional);
void         ignis_buf_clear(IgnisBuffer* buf);

void         ignis_buf_drop(void* obj);

// =============================================================================
// Unknown constructors - primitives
// =============================================================================

IgnisUnknown ignis_unknown_i8(i8 v);
IgnisUnknown ignis_unknown_i16(i16 v);
IgnisUnknown ignis_unknown_i32(i32 v);
IgnisUnknown ignis_unknown_i64(i64 v);
IgnisUnknown ignis_unknown_u8(u8 v);
IgnisUnknown ignis_unknown_u16(u16 v);
IgnisUnknown ignis_unknown_u32(u32 v);
IgnisUnknown ignis_unknown_u64(u64 v);
IgnisUnknown ignis_unknown_f32(f32 v);
IgnisUnknown ignis_unknown_f64(f64 v);
IgnisUnknown ignis_unknown_bool(boolean v);

// =============================================================================
// Unknown constructors - pointers
// =============================================================================

// Heap-managed object (owned). Reads type_id from header.
IgnisUnknown ignis_unknown_obj(void* obj);

// Raw pointer (borrowed/unsafe). Never dropped.
IgnisUnknown ignis_unknown_rawptr(void* ptr, IgnisTypeId type_id);

// =============================================================================
// Unknown accessors
// =============================================================================

IgnisTypeId ignis_typeof(IgnisUnknown v);

// Extraction helpers (no type checking - caller responsible)
i64      ignis_unknown_as_i64(IgnisUnknown v);
u64      ignis_unknown_as_u64(IgnisUnknown v);
f64      ignis_unknown_as_f64(IgnisUnknown v);
f32      ignis_unknown_as_f32(IgnisUnknown v);
void*    ignis_unknown_as_ptr(IgnisUnknown v);
boolean  ignis_unknown_is_null(IgnisUnknown v);
