/**
 * Smoke tests for the Ignis runtime.
 */

#include "../ignis_rt.h"
#include <assert.h>
#include <signal.h>
#include <stdint.h>
#include <sys/resource.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

static void test_string_basic(void) {
  IgnisString s = ignis_string_from_cstr("hello");
  assert(ignis_string_len(&s) == 5);
  assert(strcmp(ignis_string_cstr(&s), "hello") == 0);
  ignis_string_drop(&s);
}

static void test_string_concat(void) {
  IgnisString a = ignis_string_from_cstr("foo");
  IgnisString b = ignis_string_from_cstr("bar");
  IgnisString c = ignis_string_concat(&a, &b);

  assert(ignis_string_len(&c) == 6);
  assert(strcmp(ignis_string_cstr(&c), "foobar") == 0);

  ignis_string_drop(&a);
  ignis_string_drop(&b);
  ignis_string_drop(&c);
}

static void test_string_substring(void) {
  IgnisString s = ignis_string_from_cstr("hello world");
  IgnisString sub = ignis_string_substring(&s, 0, 5);

  assert(ignis_string_len(&sub) == 5);
  assert(strcmp(ignis_string_cstr(&sub), "hello") == 0);

  ignis_string_drop(&s);
  ignis_string_drop(&sub);
}

static void test_string_index_of(void) {
  IgnisString haystack = ignis_string_from_cstr("hello world");
  IgnisString needle = ignis_string_from_cstr("world");

  i64 idx = ignis_string_index_of(&haystack, &needle);
  assert(idx == 6);

  ignis_string_drop(&haystack);
  ignis_string_drop(&needle);
}

static void test_string_to_upper_lower(void) {
  IgnisString s = ignis_string_from_cstr("Hello");
  IgnisString upper = ignis_string_to_upper(&s);
  IgnisString lower = ignis_string_to_lower(&s);

  assert(strcmp(ignis_string_cstr(&upper), "HELLO") == 0);
  assert(strcmp(ignis_string_cstr(&lower), "hello") == 0);

  ignis_string_drop(&s);
  ignis_string_drop(&upper);
  ignis_string_drop(&lower);
}

static void test_number_to_string(void) {
  IgnisString s1 = ignis_i32_to_string(42);
  assert(strcmp(ignis_string_cstr(&s1), "42") == 0);
  ignis_string_drop(&s1);

  IgnisString s2 = ignis_i32_to_string(-123);
  assert(strcmp(ignis_string_cstr(&s2), "-123") == 0);
  ignis_string_drop(&s2);

  IgnisString s3 = ignis_u64_to_string(1000000);
  assert(strcmp(ignis_string_cstr(&s3), "1000000") == 0);
  ignis_string_drop(&s3);
}

static void test_memory_alloc(void) {
  void *p = ignis_alloc(64);
  assert(p != NULL);

  void *p2 = ignis_realloc(p, 128);
  assert(p2 != NULL);

  ignis_free(p2);
}

static void test_memory_aligned_alloc(void) {
  void *zero = ignis_alloc_aligned(0, 8);
  assert(zero == NULL);

  void *ptr = ignis_alloc_aligned(24, 32);
  assert(ptr != NULL);
  assert(((uintptr_t)ptr % 32) == 0);

  void *zeroed = ignis_calloc_aligned(4, sizeof(uint32_t), 16);
  assert(zeroed != NULL);
  assert(((uintptr_t)zeroed % 16) == 0);
  for (size_t i = 0; i < 4; i++) {
    assert(((uint32_t *)zeroed)[i] == 0);
  }

  ignis_free(ptr);
  ignis_free(zeroed);
}

static void test_memory_alloc_oom_aborts(void) {
  pid_t child = fork();
  assert(child >= 0);

  if (child == 0) {
    struct rlimit limit = {
      .rlim_cur = 1024 * 1024,
      .rlim_max = 1024 * 1024,
    };
    assert(setrlimit(RLIMIT_AS, &limit) == 0);

    (void)ignis_alloc(64 * 1024 * 1024);
    _exit(0);
  }

  int status = 0;
  pid_t waited = waitpid(child, &status, 0);
  assert(waited == child);
  assert(WIFSIGNALED(status));
  assert(WTERMSIG(status) == SIGABRT);
}

static void test_arena_allocator_reset_and_growth(void) {
  IgnisArena *arena = ignis_arena_create(64);
  assert(arena != NULL);

  void *first = ignis_arena_allocate(arena, 24, 8);
  void *second = ignis_arena_allocate(arena, 24, 8);
  assert(first != NULL);
  assert(second != NULL);
  assert(first != second);

  ignis_arena_reset(arena);

  void *reused = ignis_arena_allocate(arena, 24, 8);
  assert(reused == first);

  void *large = ignis_arena_allocate(arena, 256, 16);
  assert(large != NULL);
  assert(((uintptr_t)large % 16) == 0);

  ignis_arena_destroy(arena);
}

static void test_fnv_hash_cstr(void) {
  u64 first = ignis_hash_fnv1a_cstr(14695981039346656037ULL, "abc");
  u64 second = ignis_hash_fnv1a_cstr(14695981039346656037ULL, "abc");
  u64 different = ignis_hash_fnv1a_cstr(14695981039346656037ULL, "abd");

  assert(first == second);
  assert(first != 14695981039346656037ULL);
  assert(first != different);
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
  IgnisRcBox *rc = ignis_rc_alloc(sizeof(int), _Alignof(int), NULL);
  assert(rc != NULL);
  assert(ignis_rc_count(rc) == 1);

  int *payload = (int *)ignis_rc_get(rc);
  assert(payload != NULL);

  *payload = 42;
  assert(*(int *)ignis_rc_get(rc) == 42);

  ignis_rc_release(rc);
}

static void test_rc_retain_release(void) {
  IgnisRcBox *rc = ignis_rc_alloc(sizeof(int), _Alignof(int), NULL);
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

  IgnisRcBox *rc = ignis_rc_alloc(sizeof(int), _Alignof(int), test_drop_fn);
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

  IgnisRcBox *rc = ignis_rc_alloc(sizeof(int) * 4, _Alignof(int), test_drop_fn);
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
  IgnisRcBox *rc = ignis_rc_alloc(0, _Alignof(max_align_t), NULL);
  assert(rc != NULL);
  assert(ignis_rc_count(rc) == 1);
  ignis_rc_release(rc);
}

// =============================================================================
// Rc tests with memory stats (leak detection)
// =============================================================================

static void test_rc_multiple_alive_no_leak(void) {
  ignis_mem_reset_stats();

  IgnisRcBox *a = ignis_rc_alloc(sizeof(int), _Alignof(int), NULL);
  IgnisRcBox *b = ignis_rc_alloc(sizeof(int), _Alignof(int), NULL);
  IgnisRcBox *c = ignis_rc_alloc(sizeof(int), _Alignof(int), NULL);

  *(int *)ignis_rc_get(a) = 1;
  *(int *)ignis_rc_get(b) = 2;
  *(int *)ignis_rc_get(c) = 3;

  IgnisMemStats mid = ignis_mem_stats();
  assert(mid.allocs_live == 3);

  ignis_rc_release(a);
  ignis_rc_release(b);
  ignis_rc_release(c);

  IgnisMemStats end = ignis_mem_stats();
  assert(end.allocs_live == 0);
  assert(end.bytes_live == 0);
}

static void test_rc_shared_no_leak(void) {
  ignis_mem_reset_stats();

  IgnisRcBox *rc = ignis_rc_alloc(sizeof(int), _Alignof(int), NULL);
  *(int *)ignis_rc_get(rc) = 42;

  // Simulate three "copies" (like let b = a; let c = a;)
  ignis_rc_retain(rc);
  ignis_rc_retain(rc);
  assert(ignis_rc_count(rc) == 3);

  ignis_rc_release(rc);
  ignis_rc_release(rc);
  ignis_rc_release(rc);

  IgnisMemStats end = ignis_mem_stats();
  assert(end.allocs_live == 0);
  assert(end.bytes_live == 0);
}

static int nested_drop_released = 0;

static void nested_inner_drop(void *payload) {
  IgnisRcBox **inner = (IgnisRcBox **)payload;
  ignis_rc_release(*inner);
  nested_drop_released = 1;
}

static void test_rc_nested_drop_no_leak(void) {
  ignis_mem_reset_stats();
  nested_drop_released = 0;

  // Inner Rc: plain i32
  IgnisRcBox *inner = ignis_rc_alloc(sizeof(int), _Alignof(int), NULL);
  *(int *)ignis_rc_get(inner) = 99;

  // Outer Rc: holds a pointer to inner, drop_fn releases inner
  IgnisRcBox *outer = ignis_rc_alloc(sizeof(IgnisRcBox *), _Alignof(IgnisRcBox *), nested_inner_drop);
  *(IgnisRcBox **)ignis_rc_get(outer) = inner;

  IgnisMemStats mid = ignis_mem_stats();
  assert(mid.allocs_live == 2);

  // Releasing outer should cascade-release inner via the drop_fn
  ignis_rc_release(outer);

  assert(nested_drop_released == 1);

  IgnisMemStats end = ignis_mem_stats();
  assert(end.allocs_live == 0);
  assert(end.bytes_live == 0);
}

static void test_rc_interleaved_retain_release_no_leak(void) {
  ignis_mem_reset_stats();

  IgnisRcBox *rc = ignis_rc_alloc(sizeof(int), _Alignof(int), NULL);

  ignis_rc_retain(rc);   // 2
  ignis_rc_retain(rc);   // 3
  ignis_rc_release(rc);  // 2
  ignis_rc_retain(rc);   // 3
  ignis_rc_release(rc);  // 2
  ignis_rc_release(rc);  // 1

  assert(ignis_rc_count(rc) == 1);

  ignis_rc_release(rc);  // 0 — freed

  IgnisMemStats end = ignis_mem_stats();
  assert(end.allocs_live == 0);
  assert(end.bytes_live == 0);
}

static void test_rc_payload_alignment(void) {
  IgnisRcBox *rc = ignis_rc_alloc(sizeof(double), _Alignof(double), NULL);
  void *payload = ignis_rc_get(rc);

  // Payload must be aligned to max_align_t (at least 8 bytes on all platforms)
  assert(((uintptr_t)payload % _Alignof(max_align_t)) == 0);

  *(double *)payload = 3.14159;
  assert(*(double *)ignis_rc_get(rc) == 3.14159);

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
  test_memory_aligned_alloc();
  test_memory_alloc_oom_aborts();
  test_arena_allocator_reset_and_growth();
  test_fnv_hash_cstr();
  test_memcpy_memmove();
  test_rc_alloc_and_get();
  test_rc_retain_release();
  test_rc_drop_fn_called();
  test_rc_payload_with_drop();
  test_rc_zero_payload();
  test_rc_multiple_alive_no_leak();
  test_rc_shared_no_leak();
  test_rc_nested_drop_no_leak();
  test_rc_interleaved_retain_release_no_leak();
  test_rc_payload_alignment();
  return 0;
}
