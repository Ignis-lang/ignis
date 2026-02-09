#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>

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
typedef char *string;

/* Minimal string runtime stubs for E2E tests */
static inline string ignis_string_from_cstr(const char *s) {
  size_t len = strlen(s);
  char *dup = (char *)malloc(len + 1);
  memcpy(dup, s, len + 1);
  return dup;
}

static inline void ignis_string_drop(string s) {
  free(s);
}
