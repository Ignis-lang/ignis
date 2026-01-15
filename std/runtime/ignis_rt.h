#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

/**
 * Primitive type aliases used by the runtime ABI.
 */

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

/**
 * Opaque runtime type identifier.
 */

typedef u32 IgnisTypeId;

/**
 * Primitive type IDs (match types/types.c).
 */
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

/**
 * Heap object type IDs.
 */
#define IGNIS_TYPE_BUFFER_ID   100

/**
 * Pointer type ID.
 */
#define IGNIS_TYPE_PTR_ID      200

/**
 * Header for heap-managed runtime objects.
 *
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
    char* data;
    size_t len;
    size_t cap;
} IgnisString;

/**
 * `string` is an alias for a heap-managed IgnisString.
 */

typedef IgnisString* string;

/**
 * Heap-managed dynamic buffer.
 */

typedef struct {
    IgnisHeader hdr;
    void* data;
    size_t len;
    size_t cap;
    size_t elem_size;
    IgnisTypeId elem_type_id;
} IgnisBuffer;

/**
 * Null pointer alias.
 */

typedef void* null;

/**
 * Allocates `size` bytes with the runtime allocator.
 *
 * @param size Size in bytes.
 * @return Pointer to allocated memory (NULL on failure).
 */
void* ignis_alloc(size_t size);
/**
 * Resizes a previously allocated block.
 *
 * @param ptr Pointer returned by ignis_alloc or ignis_realloc.
 * @param size New size in bytes.
 * @return Pointer to resized memory (NULL on failure).
 */
void* ignis_realloc(void* ptr, size_t size);
/**
 * Allocates `count` elements of `size` bytes, zero-initialized.
 *
 * @param count Number of elements.
 * @param size Size of each element in bytes.
 * @return Pointer to allocated memory (NULL on failure).
 */
void* ignis_calloc(size_t count, size_t size);
/**
 * Frees a previously allocated block.
 *
 * @param ptr Pointer returned by ignis_alloc/ignis_realloc/ignis_calloc.
 */
void  ignis_free(void* ptr);

/**
 * Creates a new empty string.
 *
 * @return Newly allocated string (NULL on failure).
 */
IgnisString* ignis_string_new(void);
/**
 * Creates a new empty string with at least `cap` capacity.
 *
 * @param cap Minimum capacity in bytes (including terminator).
 * @return Newly allocated string (NULL on failure).
 */
IgnisString* ignis_string_with_capacity(size_t cap);
/**
 * Creates a new string from a C string.
 *
 * @param s Null-terminated string (may be NULL).
 * @return Newly allocated string (empty string if input is NULL).
 */
IgnisString* ignis_string_from_cstr(const char* s);
/**
 * Creates a new string from a byte slice.
 *
 * @param s Byte data (may be NULL if len == 0).
 * @param len Number of bytes to copy.
 * @return Newly allocated string (NULL on failure).
 */
IgnisString* ignis_string_from_len(const char* s, size_t len);
/**
 * Creates a clone of an existing string.
 *
 * @param s Source string (may be NULL).
 * @return Newly allocated clone (empty string if input is NULL).
 */
IgnisString* ignis_string_clone(const IgnisString* s);

/**
 * Appends a character to the string in place.
 *
 * @param s Target string.
 * @param c Character to append.
 */
void         ignis_string_push_char(IgnisString* s, char c);
/**
 * Appends a C string to the string in place.
 *
 * @param s Target string.
 * @param cstr Null-terminated string to append.
 */
void         ignis_string_push_cstr(IgnisString* s, const char* cstr);
/**
 * Appends another IgnisString to the string in place.
 *
 * @param s Target string.
 * @param other String to append.
 */
void         ignis_string_push_str(IgnisString* s, const IgnisString* other);

/**
 * Returns a null-terminated view of the string.
 *
 * @param s Input string.
 * @return C string pointer ("" if input is NULL).
 */
const char*  ignis_string_cstr(const IgnisString* s);
/**
 * Returns the length of the string in bytes.
 *
 * @param s Input string.
 * @return Byte length (0 if input is NULL).
 */
size_t       ignis_string_len(const IgnisString* s);
/**
 * Returns the capacity of the string buffer in bytes.
 *
 * @param s Input string.
 * @return Capacity in bytes (0 if input is NULL).
 */
size_t       ignis_string_cap(const IgnisString* s);
/**
 * Returns the character at `idx`.
 *
 * @param s Input string.
 * @param idx Zero-based index.
 * @return Character at index or '\0' if out of range.
 */
char         ignis_string_char_at(const IgnisString* s, size_t idx);

/**
 * Clears the string to length 0.
 *
 * @param s Target string.
 */
void         ignis_string_clear(IgnisString* s);
/**
 * Ensures capacity for `additional` bytes beyond current length.
 *
 * @param s Target string.
 * @param additional Additional bytes to reserve.
 */
void         ignis_string_reserve(IgnisString* s, size_t additional);

/**
 * Releases the string and its storage.
 *
 * @param s String to drop.
 */
void         ignis_string_drop(IgnisString* s);

/**
 * Creates a new buffer for elements of the given size and type ID.
 *
 * @param elem_size Size of each element in bytes.
 * @param elem_type_id Runtime type identifier for the element type.
 * @return Newly allocated buffer (NULL on failure).
 */
IgnisBuffer* ignis_buf_new(size_t elem_size, IgnisTypeId elem_type_id);
/**
 * Creates a new buffer with reserved capacity.
 *
 * @param elem_size Size of each element in bytes.
 * @param elem_type_id Runtime type identifier for the element type.
 * @param cap Initial capacity in elements.
 * @return Newly allocated buffer (NULL on failure).
 */
IgnisBuffer* ignis_buf_with_capacity(size_t elem_size, IgnisTypeId elem_type_id, size_t cap);

/**
 * Appends an element to the buffer.
 *
 * @param buf Target buffer.
 * @param elem Pointer to element bytes to copy.
 */
void         ignis_buf_push(IgnisBuffer* buf, const void* elem);
/**
 * Returns a mutable pointer to the element at `idx`.
 *
 * @param buf Target buffer.
 * @param idx Zero-based index.
 * @return Pointer to the element or NULL if out of range.
 */
void*        ignis_buf_at(IgnisBuffer* buf, size_t idx);
/**
 * Returns a const pointer to the element at `idx`.
 *
 * @param buf Target buffer.
 * @param idx Zero-based index.
 * @return Pointer to the element or NULL if out of range.
 */
const void*  ignis_buf_at_const(const IgnisBuffer* buf, size_t idx);
/**
 * Returns the number of elements in the buffer.
 *
 * @param buf Target buffer.
 * @return Element count.
 */
size_t       ignis_buf_len(const IgnisBuffer* buf);
/**
 * Returns the capacity of the buffer in elements.
 *
 * @param buf Target buffer.
 * @return Capacity in elements.
 */
size_t       ignis_buf_cap(const IgnisBuffer* buf);

/**
 * Resizes the buffer length to `new_len`.
 *
 * @param buf Target buffer.
 * @param new_len New length in elements.
 */
void         ignis_buf_resize(IgnisBuffer* buf, size_t new_len);
/**
 * Ensures capacity for at least `additional` more elements.
 *
 * @param buf Target buffer.
 * @param additional Additional element capacity.
 */
void         ignis_buf_reserve(IgnisBuffer* buf, size_t additional);
/**
 * Clears the buffer length to zero without freeing capacity.
 *
 * @param buf Target buffer.
 */
void         ignis_buf_clear(IgnisBuffer* buf);

/**
 * Releases the buffer and its storage.
 *
 * @param buf Buffer to drop.
 */
void         ignis_buf_drop(IgnisBuffer* buf);
