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

// =============================================================================
// Rc tests
// =============================================================================

static void test_rc_alloc_and_get(void) {
  IgnisRcBox *rc = ignis_rc_alloc(sizeof(int), NULL);
  assert(rc != NULL);
  assert(ignis_rc_count(rc) == 1);

  int *payload = (int *)ignis_rc_get(rc);
  assert(payload != NULL);

  *payload = 42;
  assert(*(int *)ignis_rc_get(rc) == 42);

  ignis_rc_release(rc);
}

static void test_rc_retain_release(void) {
  IgnisRcBox *rc = ignis_rc_alloc(sizeof(int), NULL);
  assert(ignis_rc_count(rc) == 1);

  ignis_rc_retain(rc);
  assert(ignis_rc_count(rc) == 2);

  ignis_rc_retain(rc);
  assert(ignis_rc_count(rc) == 3);

  ignis_rc_release(rc);
  assert(ignis_rc_count(rc) == 2);

  ignis_rc_release(rc);
  assert(ignis_rc_count(rc) == 1);

  // Final release frees the allocation
  ignis_rc_release(rc);
}

static int drop_called_count = 0;

static void test_drop_fn(void *payload) {
  (void)payload;
  drop_called_count += 1;
}

static void test_rc_drop_fn_called(void) {
  drop_called_count = 0;

  IgnisRcBox *rc = ignis_rc_alloc(sizeof(int), test_drop_fn);
  ignis_rc_retain(rc);

  // First release should NOT call drop (refcount goes to 1)
  ignis_rc_release(rc);
  assert(drop_called_count == 0);

  // Second release should call drop (refcount goes to 0)
  ignis_rc_release(rc);
  assert(drop_called_count == 1);
}

static void test_rc_payload_with_drop(void) {
  drop_called_count = 0;

  IgnisRcBox *rc = ignis_rc_alloc(sizeof(int) * 4, test_drop_fn);
  int *arr = (int *)ignis_rc_get(rc);

  arr[0] = 10;
  arr[1] = 20;
  arr[2] = 30;
  arr[3] = 40;

  assert(arr[0] == 10);
  assert(arr[3] == 40);

  ignis_rc_release(rc);
  assert(drop_called_count == 1);
}

static void test_rc_zero_payload(void) {
  IgnisRcBox *rc = ignis_rc_alloc(0, NULL);
  assert(rc != NULL);
  assert(ignis_rc_count(rc) == 1);
  ignis_rc_release(rc);
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
  test_rc_alloc_and_get();
  test_rc_retain_release();
  test_rc_drop_fn_called();
  test_rc_payload_with_drop();
  test_rc_zero_payload();
  return 0;
}
