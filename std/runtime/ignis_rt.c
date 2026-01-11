#include "ignis_rt.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// =============================================================================
// Alloc/Free base
// =============================================================================

void* ignis_alloc(size_t size) {
    return malloc(size);
}

void* ignis_realloc(void* ptr, size_t size) {
    return realloc(ptr, size);
}

void* ignis_calloc(size_t count, size_t size) {
    return calloc(count, size);
}

void ignis_free(void* ptr) {
    free(ptr);
}

// =============================================================================
// Drop generic
// =============================================================================

void ignis_drop_obj(void* obj) {
    if (obj == NULL) {
        return;
    }

    IgnisHeader* hdr = (IgnisHeader*)obj;

    if (hdr->drop_fn != NULL) {
        hdr->drop_fn(obj);
    }
}

void ignis_drop_unknown(IgnisUnknown v) {
    if ((v.flags & IGNIS_UF_OWNED) &&
        (v.flags & IGNIS_UF_HEAP) &&
        v.payload.ptr_val != NULL) {
        ignis_drop_obj(v.payload.ptr_val);
    }
}

// =============================================================================
// String - internal helpers
// =============================================================================

static void ignis_string_grow(IgnisString* s, size_t min_cap) {
    if (s == NULL) {
        return;
    }

    size_t new_cap = s->cap;
    if (new_cap == 0) {
        new_cap = 16;
    }

    while (new_cap < min_cap) {
        new_cap *= 2;
    }

    char* new_data = (char*)ignis_realloc(s->data, new_cap);
    if (new_data == NULL) {
        return;
    }

    s->data = new_data;
    s->cap = new_cap;
}

// =============================================================================
// String API
// =============================================================================

IgnisString* ignis_string_new(void) {
    return ignis_string_with_capacity(16);
}

IgnisString* ignis_string_with_capacity(size_t cap) {
    IgnisString* s = (IgnisString*)ignis_alloc(sizeof(IgnisString));
    if (s == NULL) {
        return NULL;
    }

    // Ensure capacity for at least null terminator
    if (cap < 1) {
        cap = 1;
    }

    s->hdr.type_id = IGNIS_TYPE_STRING_ID;
    s->hdr.refcnt = 1;
    s->hdr.drop_fn = ignis_string_drop;

    s->data = (char*)ignis_alloc(cap);
    if (s->data == NULL) {
        ignis_free(s);
        return NULL;
    }

    s->data[0] = '\0';
    s->len = 0;
    s->cap = cap;

    return s;
}

IgnisString* ignis_string_from_cstr(const char* cstr) {
    if (cstr == NULL) {
        return ignis_string_new();
    }

    size_t len = strlen(cstr);
    return ignis_string_from_len(cstr, len);
}

IgnisString* ignis_string_from_len(const char* s, size_t len) {
    // Capacity: len + 1 for null terminator
    size_t cap = len + 1;
    IgnisString* str = ignis_string_with_capacity(cap);
    if (str == NULL) {
        return NULL;
    }

    if (s != NULL && len > 0) {
        memcpy(str->data, s, len);
    }
    str->data[len] = '\0';
    str->len = len;

    return str;
}

IgnisString* ignis_string_clone(const IgnisString* s) {
    if (s == NULL) {
        return ignis_string_new();
    }
    return ignis_string_from_len(s->data, s->len);
}

void ignis_string_push_char(IgnisString* s, char c) {
    if (s == NULL) {
        return;
    }

    // Need space for: existing chars + new char + null terminator
    size_t required = s->len + 2;
    if (required > s->cap) {
        ignis_string_grow(s, required);
        if (s->cap < required) {
            return;
        }
    }

    s->data[s->len] = c;
    s->len++;
    s->data[s->len] = '\0';
}

void ignis_string_push_cstr(IgnisString* s, const char* cstr) {
    if (s == NULL || cstr == NULL) {
        return;
    }

    size_t cstr_len = strlen(cstr);
    if (cstr_len == 0) {
        return;
    }

    size_t required = s->len + cstr_len + 1;
    if (required > s->cap) {
        ignis_string_grow(s, required);
        if (s->cap < required) {
            return;
        }
    }

    memcpy(s->data + s->len, cstr, cstr_len);
    s->len += cstr_len;
    s->data[s->len] = '\0';
}

void ignis_string_push_str(IgnisString* s, const IgnisString* other) {
    if (s == NULL || other == NULL || other->len == 0) {
        return;
    }

    size_t required = s->len + other->len + 1;
    if (required > s->cap) {
        ignis_string_grow(s, required);
        if (s->cap < required) {
            return;
        }
    }

    memcpy(s->data + s->len, other->data, other->len);
    s->len += other->len;
    s->data[s->len] = '\0';
}

const char* ignis_string_cstr(const IgnisString* s) {
    if (s == NULL || s->data == NULL) {
        return "";
    }
    return s->data;
}

size_t ignis_string_len(const IgnisString* s) {
    if (s == NULL) {
        return 0;
    }
    return s->len;
}

size_t ignis_string_cap(const IgnisString* s) {
    if (s == NULL) {
        return 0;
    }
    return s->cap;
}

char ignis_string_char_at(const IgnisString* s, size_t idx) {
    if (s == NULL || idx >= s->len) {
        return '\0';
    }
    return s->data[idx];
}

void ignis_string_clear(IgnisString* s) {
    if (s == NULL) {
        return;
    }
    s->len = 0;
    if (s->data != NULL) {
        s->data[0] = '\0';
    }
}

void ignis_string_reserve(IgnisString* s, size_t additional) {
    if (s == NULL) {
        return;
    }

    size_t required = s->len + additional + 1;
    if (required > s->cap) {
        ignis_string_grow(s, required);
    }
}

void ignis_string_drop(void* obj) {
    if (obj == NULL) {
        return;
    }

    IgnisString* s = (IgnisString*)obj;

    if (s->data != NULL) {
        ignis_free(s->data);
        s->data = NULL;
    }

    s->len = 0;
    s->cap = 0;

    ignis_free(s);
}

// =============================================================================
// Buffer - internal helpers
// =============================================================================

static void ignis_buf_grow(IgnisBuffer* buf, size_t min_cap) {
    if (buf == NULL || buf->elem_size == 0) {
        return;
    }

    size_t new_cap = buf->cap;
    if (new_cap == 0) {
        new_cap = 8;
    }

    while (new_cap < min_cap) {
        new_cap *= 2;
    }

    void* new_data = ignis_realloc(buf->data, new_cap * buf->elem_size);
    if (new_data == NULL) {
        return;
    }

    buf->data = new_data;
    buf->cap = new_cap;
}

// =============================================================================
// Buffer API
// =============================================================================

IgnisBuffer* ignis_buf_new(size_t elem_size, IgnisTypeId elem_type_id) {
    return ignis_buf_with_capacity(elem_size, elem_type_id, 8);
}

IgnisBuffer* ignis_buf_with_capacity(size_t elem_size, IgnisTypeId elem_type_id, size_t cap) {
    if (elem_size == 0) {
        return NULL;
    }

    IgnisBuffer* buf = (IgnisBuffer*)ignis_alloc(sizeof(IgnisBuffer));
    if (buf == NULL) {
        return NULL;
    }

    buf->hdr.type_id = IGNIS_TYPE_BUFFER_ID;
    buf->hdr.refcnt = 1;
    buf->hdr.drop_fn = ignis_buf_drop;

    buf->elem_size = elem_size;
    buf->elem_type_id = elem_type_id;
    buf->len = 0;
    buf->cap = cap;

    if (cap > 0) {
        buf->data = ignis_alloc(cap * elem_size);
        if (buf->data == NULL) {
            ignis_free(buf);
            return NULL;
        }
    } else {
        buf->data = NULL;
    }

    return buf;
}

void ignis_buf_push(IgnisBuffer* buf, const void* elem) {
    if (buf == NULL || elem == NULL) {
        return;
    }

    if (buf->len >= buf->cap) {
        ignis_buf_grow(buf, buf->len + 1);
        if (buf->len >= buf->cap) {
            return;
        }
    }

    void* dest = (char*)buf->data + (buf->len * buf->elem_size);
    memcpy(dest, elem, buf->elem_size);
    buf->len++;
}

void* ignis_buf_at(IgnisBuffer* buf, size_t idx) {
    if (buf == NULL || idx >= buf->len) {
        return NULL;
    }
    return (char*)buf->data + (idx * buf->elem_size);
}

const void* ignis_buf_at_const(const IgnisBuffer* buf, size_t idx) {
    if (buf == NULL || idx >= buf->len) {
        return NULL;
    }
    return (const char*)buf->data + (idx * buf->elem_size);
}

size_t ignis_buf_len(const IgnisBuffer* buf) {
    if (buf == NULL) {
        return 0;
    }
    return buf->len;
}

size_t ignis_buf_cap(const IgnisBuffer* buf) {
    if (buf == NULL) {
        return 0;
    }
    return buf->cap;
}

void ignis_buf_resize(IgnisBuffer* buf, size_t new_len) {
    if (buf == NULL) {
        return;
    }

    if (new_len > buf->cap) {
        ignis_buf_grow(buf, new_len);
        if (buf->cap < new_len) {
            return;
        }
    }

    // If growing, zero-initialize new elements
    if (new_len > buf->len) {
        void* start = (char*)buf->data + (buf->len * buf->elem_size);
        size_t bytes = (new_len - buf->len) * buf->elem_size;
        memset(start, 0, bytes);
    }

    buf->len = new_len;
}

void ignis_buf_reserve(IgnisBuffer* buf, size_t additional) {
    if (buf == NULL) {
        return;
    }

    size_t required = buf->len + additional;
    if (required > buf->cap) {
        ignis_buf_grow(buf, required);
    }
}

void ignis_buf_clear(IgnisBuffer* buf) {
    if (buf == NULL) {
        return;
    }
    buf->len = 0;
}

void ignis_buf_drop(void* obj) {
    if (obj == NULL) {
        return;
    }

    IgnisBuffer* buf = (IgnisBuffer*)obj;

    if (buf->data != NULL) {
        ignis_free(buf->data);
        buf->data = NULL;
    }

    buf->len = 0;
    buf->cap = 0;

    ignis_free(buf);
}

// =============================================================================
// Unknown constructors - primitives
// =============================================================================

IgnisUnknown ignis_unknown_i8(i8 v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_I8_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.i64_val = (i64)v;
    return u;
}

IgnisUnknown ignis_unknown_i16(i16 v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_I16_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.i64_val = (i64)v;
    return u;
}

IgnisUnknown ignis_unknown_i32(i32 v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_I32_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.i64_val = (i64)v;
    return u;
}

IgnisUnknown ignis_unknown_i64(i64 v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_I64_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.i64_val = v;
    return u;
}

IgnisUnknown ignis_unknown_u8(u8 v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_U8_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.u64_val = (u64)v;
    return u;
}

IgnisUnknown ignis_unknown_u16(u16 v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_U16_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.u64_val = (u64)v;
    return u;
}

IgnisUnknown ignis_unknown_u32(u32 v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_U32_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.u64_val = (u64)v;
    return u;
}

IgnisUnknown ignis_unknown_u64(u64 v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_U64_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.u64_val = v;
    return u;
}

IgnisUnknown ignis_unknown_f32(f32 v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_F32_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.f32_val = v;
    return u;
}

IgnisUnknown ignis_unknown_f64(f64 v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_F64_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.f64_val = v;
    return u;
}

IgnisUnknown ignis_unknown_bool(boolean v) {
    IgnisUnknown u;
    u.type_id = IGNIS_TYPE_BOOL_ID;
    u.flags = IGNIS_UF_NONE;
    u.payload.u64_val = v ? 1 : 0;
    return u;
}

// =============================================================================
// Unknown constructors - pointers
// =============================================================================

IgnisUnknown ignis_unknown_obj(void* obj) {
    IgnisUnknown u;

    if (obj == NULL) {
        u.type_id = IGNIS_TYPE_UNKNOWN_ID;
        u.flags = IGNIS_UF_NONE;
        u.payload.ptr_val = NULL;
        return u;
    }

    IgnisHeader* hdr = (IgnisHeader*)obj;
    u.type_id = hdr->type_id;
    u.flags = IGNIS_UF_OWNED | IGNIS_UF_HEAP;
    u.payload.ptr_val = obj;

    return u;
}

IgnisUnknown ignis_unknown_rawptr(void* ptr, IgnisTypeId type_id) {
    IgnisUnknown u;
    u.type_id = type_id;
    u.flags = IGNIS_UF_RAW;
    u.payload.ptr_val = ptr;
    return u;
}

// =============================================================================
// Unknown accessors
// =============================================================================

IgnisTypeId ignis_typeof(IgnisUnknown v) {
    return v.type_id;
}

i64 ignis_unknown_as_i64(IgnisUnknown v) {
    return v.payload.i64_val;
}

u64 ignis_unknown_as_u64(IgnisUnknown v) {
    return v.payload.u64_val;
}

f64 ignis_unknown_as_f64(IgnisUnknown v) {
    return v.payload.f64_val;
}

f32 ignis_unknown_as_f32(IgnisUnknown v) {
    return v.payload.f32_val;
}

void* ignis_unknown_as_ptr(IgnisUnknown v) {
    return v.payload.ptr_val;
}

boolean ignis_unknown_is_null(IgnisUnknown v) {
    // For pointer types, check if ptr is NULL
    if ((v.flags & IGNIS_UF_HEAP) || (v.flags & IGNIS_UF_RAW)) {
        return v.payload.ptr_val == NULL ? TRUE : FALSE;
    }
    return FALSE;
}
