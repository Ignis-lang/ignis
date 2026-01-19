/**
 * Internal header for Ignis runtime implementation.
 *
 * This file is NOT part of the public API. It provides shared helpers,
 * macros, and standard includes for the rt_*.c implementation files.
 */

#pragma once

#include <ctype.h>
#include <float.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Compiler hints for branch prediction
#ifdef __GNUC__
#define IGNIS_UNLIKELY(x) __builtin_expect(!!(x), 0)
#define IGNIS_LIKELY(x) __builtin_expect(!!(x), 1)
#else
#define IGNIS_UNLIKELY(x) (x)
#define IGNIS_LIKELY(x) (x)
#endif

#define IGNIS_STATIC_INLINE static inline

/**
 * Checked multiplication that returns 0 on overflow.
 *
 * @param a First operand.
 * @param b Second operand.
 * @return Product, or 0 if overflow would occur.
 */
IGNIS_STATIC_INLINE size_t ignis_checked_mul(size_t a, size_t b) {
  if (a == 0 || b == 0) {
    return 0;
  }

  if (a > SIZE_MAX / b) {
    return 0;
  }

  return a * b;
}

/**
 * Rounds n up to the next multiple of align.
 *
 * @param n Value to align.
 * @param align Alignment (must be power of 2).
 * @return Aligned value.
 */
IGNIS_STATIC_INLINE size_t ignis_align_up(size_t n, size_t align) {
  return (n + align - 1) & ~(align - 1);
}
