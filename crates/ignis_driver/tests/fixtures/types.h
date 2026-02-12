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
typedef u32 ignis_atom_t;
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

/* Minimal Rc/Weak runtime stubs for E2E tests.
 *
 * Ignis emits function-typed parameters as void* in the generated C,
 * so all stubs use void* for drop_fn to match the codegen signatures.
 */
typedef void (*IgnisDropFn)(void *);

typedef struct {
  _Atomic int refcount;
  _Atomic int weak_count;
  IgnisDropFn drop_fn;
  size_t payload_size;
  _Alignas(16) unsigned char payload[];
} IgnisRcBox;

static inline void *ignis_rc_alloc(u64 payload_size, u64 payload_align, void *drop_fn) {
  (void)payload_align;
  IgnisRcBox *box = (IgnisRcBox *)calloc(1, sizeof(IgnisRcBox) + (size_t)payload_size);
  box->refcount = 1;
  box->weak_count = 0;
  box->drop_fn = (IgnisDropFn)drop_fn;
  box->payload_size = (size_t)payload_size;
  return (void *)box;
}

static inline void ignis_rc_retain(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  box->refcount++;
}

static inline void ignis_rc_release(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  if (--box->refcount == 0) {
    if (box->drop_fn) box->drop_fn(box->payload);
    if (box->weak_count == 0) free(box);
  }
}

static inline void *ignis_rc_get(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  return box->payload;
}

static inline u32 ignis_rc_count(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  return (u32)box->refcount;
}

static inline void ignis_rc_downgrade(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  box->weak_count++;
}

static inline void *ignis_rc_upgrade(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  if (box->refcount == 0) return NULL;
  box->refcount++;
  return handle;
}

static inline void ignis_weak_retain(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  box->weak_count++;
}

static inline void ignis_weak_release(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  if (--box->weak_count == 0 && box->refcount == 0) {
    free(box);
  }
}

static inline u32 ignis_weak_count(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  return (u32)box->weak_count;
}

/* Custom Rc hook wrappers used by std=false/custom-runtime E2E tests */
static int ignis_test_rc_retain_calls = 0;
static int ignis_test_rc_release_calls = 0;

static inline void ignis_test_rc_reset_stats(void) {
  ignis_test_rc_retain_calls = 0;
  ignis_test_rc_release_calls = 0;
}

static inline int ignis_test_rc_retain_count(void) {
  return ignis_test_rc_retain_calls;
}

static inline int ignis_test_rc_release_count(void) {
  return ignis_test_rc_release_calls;
}

static inline void *custom_rc_alloc(uint64_t payload_size, uint64_t payload_align, void *drop_fn) {
  (void)payload_align;
  return (void *)ignis_rc_alloc(payload_size, payload_align, drop_fn);
}

static inline void *custom_rc_get(void *handle) {
  return ignis_rc_get((IgnisRcBox *)handle);
}

static inline void custom_rc_retain(void *handle) {
  ignis_test_rc_retain_calls += 1;
  ignis_rc_retain((IgnisRcBox *)handle);
}

static inline void custom_rc_release(void *handle) {
  ignis_test_rc_release_calls += 1;
  ignis_rc_release((IgnisRcBox *)handle);
}
