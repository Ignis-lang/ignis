#include "number.h"
#include <stdio.h>

#define INT_BUF_SIZE 24
#define FLOAT_BUF_SIZE 32

// =============================================================================
// Integer to string conversions
// =============================================================================

string i8ToString(i8 value) {
    char buf[INT_BUF_SIZE];
    int len = snprintf(buf, INT_BUF_SIZE, "%d", (int)value);
    return ignis_string_from_len(buf, (size_t)len);
}

string i16ToString(i16 value) {
    char buf[INT_BUF_SIZE];
    int len = snprintf(buf, INT_BUF_SIZE, "%d", (int)value);
    return ignis_string_from_len(buf, (size_t)len);
}

string i32ToString(i32 value) {
    char buf[INT_BUF_SIZE];
    int len = snprintf(buf, INT_BUF_SIZE, "%d", value);
    return ignis_string_from_len(buf, (size_t)len);
}

string i64ToString(i64 value) {
    char buf[INT_BUF_SIZE];
    int len = snprintf(buf, INT_BUF_SIZE, "%ld", value);
    return ignis_string_from_len(buf, (size_t)len);
}

string u8ToString(u8 value) {
    char buf[INT_BUF_SIZE];
    int len = snprintf(buf, INT_BUF_SIZE, "%u", (unsigned int)value);
    return ignis_string_from_len(buf, (size_t)len);
}

string u16ToString(u16 value) {
    char buf[INT_BUF_SIZE];
    int len = snprintf(buf, INT_BUF_SIZE, "%u", (unsigned int)value);
    return ignis_string_from_len(buf, (size_t)len);
}

string u32ToString(u32 value) {
    char buf[INT_BUF_SIZE];
    int len = snprintf(buf, INT_BUF_SIZE, "%u", value);
    return ignis_string_from_len(buf, (size_t)len);
}

string u64ToString(u64 value) {
    char buf[INT_BUF_SIZE];
    int len = snprintf(buf, INT_BUF_SIZE, "%lu", value);
    return ignis_string_from_len(buf, (size_t)len);
}

// =============================================================================
// Float to string conversions
// =============================================================================

string f32ToString(f32 value) {
    char buf[FLOAT_BUF_SIZE];
    int len = snprintf(buf, FLOAT_BUF_SIZE, "%g", (double)value);
    return ignis_string_from_len(buf, (size_t)len);
}

string f64ToString(f64 value) {
    char buf[FLOAT_BUF_SIZE];
    int len = snprintf(buf, FLOAT_BUF_SIZE, "%g", value);
    return ignis_string_from_len(buf, (size_t)len);
}

// =============================================================================
// Boolean to string
// =============================================================================

string booleanToString(boolean value) {
    if (value) {
        return ignis_string_from_cstr("true");
    } else {
        return ignis_string_from_cstr("false");
    }
}

// =============================================================================
// Empty string
// =============================================================================

string stringEmpty(void) {
    return ignis_string_new();
}
