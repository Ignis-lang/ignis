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

/* Minimal Rc runtime stubs for E2E tests */
typedef void (*IgnisDropFn)(void *);

typedef struct {
  _Atomic int refcount;
  IgnisDropFn drop_fn;
  size_t payload_size;
  _Alignas(16) unsigned char payload[];
} IgnisRcBox;

static inline void *ignis_rc_alloc(size_t payload_size, size_t payload_align, IgnisDropFn drop_fn) {
  (void)payload_align;
  IgnisRcBox *box = (IgnisRcBox *)calloc(1, sizeof(IgnisRcBox) + payload_size);
  box->refcount = 1;
  box->drop_fn = drop_fn;
  box->payload_size = payload_size;
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
    free(box);
  }
}

static inline void *ignis_rc_get(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  return box->payload;
}

static inline int ignis_rc_count(void *handle) {
  IgnisRcBox *box = (IgnisRcBox *)handle;
  return box->refcount;
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
  return (void *)ignis_rc_alloc((size_t)payload_size, (size_t)payload_align, (IgnisDropFn)drop_fn);
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
