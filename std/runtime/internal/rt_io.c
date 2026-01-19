/**
 * Ignis runtime: I/O operations.
 */

#include "../ignis_rt.h"
#include "rt_internal.h"

// =============================================================================
// I/O API
// =============================================================================

void ignis_print(const IgnisString *s) {
  if (s == NULL) {
    return;
  }

  const char *cstr = ignis_string_cstr(s);
  if (cstr != NULL) {
    printf("%s", cstr);
  }
}

void ignis_eprint(const IgnisString *s) {
  if (s == NULL) {
    return;
  }

  const char *cstr = ignis_string_cstr(s);
  if (cstr != NULL) {
    fprintf(stderr, "%s", cstr);
  }
}
