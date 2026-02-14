/* Ignis runtime: I/O operations. */

#include "../ignis_rt.h"
#include "rt_internal.h"

// =============================================================================
// I/O API
// =============================================================================

void ignis_print(const IgnisString *s) {
  const char *cstr = ignis_string_cstr(s);
  if (cstr != NULL) {
    printf("%s", cstr);
  }
}

void ignis_eprint(const IgnisString *s) {
  const char *cstr = ignis_string_cstr(s);
  if (cstr != NULL) {
    fprintf(stderr, "%s", cstr);
  }
}

void ignis_println(const IgnisString *s) {
  const char *cstr = ignis_string_cstr(s);
  if (cstr != NULL) {
    printf("%s\n", cstr);
  } else {
    printf("\n");
  }
}

void ignis_eprintln(const IgnisString *s) {
  const char *cstr = ignis_string_cstr(s);
  if (cstr != NULL) {
    fprintf(stderr, "%s\n", cstr);
  } else {
    fprintf(stderr, "\n");
  }
}
