/**
 * Smoke tests for the Ignis runtime.
 */

#include "../ignis_rt.h"
#include <assert.h>
#include <string.h>

static void test_string_basic(void) {
  IgnisString *s = ignis_string_from_cstr("hello");
  assert(s != NULL);
  assert(ignis_string_len(s) == 5);
  assert(strcmp(ignis_string_cstr(s), "hello") == 0);
  ignis_string_drop(s);
}

static void test_string_concat(void) {
  IgnisString *a = ignis_string_from_cstr("foo");
  IgnisString *b = ignis_string_from_cstr("bar");
  IgnisString *c = ignis_string_concat(a, b);

  assert(c != NULL);
  assert(ignis_string_len(c) == 6);
  assert(strcmp(ignis_string_cstr(c), "foobar") == 0);

  ignis_string_drop(a);
  ignis_string_drop(b);
  ignis_string_drop(c);
}

static void test_string_substring(void) {
  IgnisString *s = ignis_string_from_cstr("hello world");
  IgnisString *sub = ignis_string_substring(s, 0, 5);

  assert(sub != NULL);
  assert(ignis_string_len(sub) == 5);
  assert(strcmp(ignis_string_cstr(sub), "hello") == 0);

  ignis_string_drop(s);
  ignis_string_drop(sub);
}

static void test_string_index_of(void) {
  IgnisString *haystack = ignis_string_from_cstr("hello world");
  IgnisString *needle = ignis_string_from_cstr("world");

  i64 idx = ignis_string_index_of(haystack, needle);
  assert(idx == 6);

  ignis_string_drop(haystack);
  ignis_string_drop(needle);
}

static void test_string_to_upper_lower(void) {
  IgnisString *s = ignis_string_from_cstr("Hello");
  IgnisString *upper = ignis_string_to_upper(s);
  IgnisString *lower = ignis_string_to_lower(s);

  assert(strcmp(ignis_string_cstr(upper), "HELLO") == 0);
  assert(strcmp(ignis_string_cstr(lower), "hello") == 0);

  ignis_string_drop(s);
  ignis_string_drop(upper);
  ignis_string_drop(lower);
}

static void test_number_to_string(void) {
  IgnisString *s1 = ignis_i32_to_string(42);
  assert(strcmp(ignis_string_cstr(s1), "42") == 0);
  ignis_string_drop(s1);

  IgnisString *s2 = ignis_i32_to_string(-123);
  assert(strcmp(ignis_string_cstr(s2), "-123") == 0);
  ignis_string_drop(s2);

  IgnisString *s3 = ignis_u64_to_string(1000000);
  assert(strcmp(ignis_string_cstr(s3), "1000000") == 0);
  ignis_string_drop(s3);
}

static void test_memory_alloc(void) {
  void *p = ignis_alloc(64);
  assert(p != NULL);

  void *p2 = ignis_realloc(p, 128);
  assert(p2 != NULL);

  ignis_free(p2);
}

static void test_memcpy_memmove(void) {
  char src[10] = "hello";
  char dest[10] = {0};

  ignis_memcpy(dest, src, 6);
  assert(strcmp(dest, "hello") == 0);

  char overlap[20] = "hello world";
  ignis_memmove(overlap + 6, overlap, 5);
  assert(strncmp(overlap + 6, "hello", 5) == 0);
}

static void test_buffer_basic(void) {
  IgnisBuffer *buf = ignis_buf_new(sizeof(i32), IGNIS_TYPE_I32_ID);
  assert(buf != NULL);
  assert(ignis_buf_len(buf) == 0);

  i32 val = 42;
  ignis_buf_push(buf, &val);
  assert(ignis_buf_len(buf) == 1);

  i32 *ptr = (i32 *)ignis_buf_at(buf, 0);
  assert(ptr != NULL);
  assert(*ptr == 42);

  ignis_buf_drop(buf);
}

int main(void) {
  test_string_basic();
  test_string_concat();
  test_string_substring();
  test_string_index_of();
  test_string_to_upper_lower();
  test_number_to_string();
  test_memory_alloc();
  test_memcpy_memmove();
  test_buffer_basic();

  return 0;
}
