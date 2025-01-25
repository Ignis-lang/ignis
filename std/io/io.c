#include "./io.h"
#include <stdarg.h>
#include <stdio.h>

void print(string format, ...) {
  va_list args;

  va_start(args, format);
  vfprintf(stdout, format, args);
  va_end(args);
}
