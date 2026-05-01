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

/* Minimal process bootstrap stub for generated wrapper tests. */
static inline void ignis_runtime_init(i32 argc, char **argv) {
  (void)argc;
  (void)argv;
}
