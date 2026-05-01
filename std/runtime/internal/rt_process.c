/* Ignis runtime: process bootstrap helpers for argv access. */

#include "../ignis_rt.h"
#include "rt_internal.h"

static i32 ignis_runtime_argc = 0;
static char **ignis_runtime_argv = NULL;

void ignis_runtime_init(i32 argc, char **argv) {
  ignis_runtime_argc = argc < 0 ? 0 : argc;
  ignis_runtime_argv = argv;
}

i32 ignis_process_arg_count(void) {
  return ignis_runtime_argc;
}

const char *ignis_process_arg_at(i32 index) {
  if (index < 0 || index >= ignis_runtime_argc || ignis_runtime_argv == NULL) {
    return NULL;
  }

  return ignis_runtime_argv[index];
}
