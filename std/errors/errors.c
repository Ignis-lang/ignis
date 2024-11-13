#include "./errors.h"
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * @brief Panic function.
 *
 * This function is designed to handle critical errors that require the program
 * to stop execution. It prints a formatted message to `stderr` and terminates
 * the program using `exit(EXIT_FAILURE)`. The message can be formatted
 * similarly to `printf`, allowing for additional details like variable values
 * to be included in the output.
 *
 * @param format A string that describes the error message to be printed.
 *        This string can include format specifiers (e.g., `%s`, `%d`) for
 *        dynamic data.
 *
 * @param ... The values that correspond to the format specifiers in the
 *        `format` string. These values are passed as a variable argument list
 *        using the `...` notation.
 *
 * @details The function uses `va_list`, `va_start`, and `va_end` to handle
 *          the variable number of arguments passed to it. The `vfprintf`
 *          function is used to print the formatted string to `stderr`, which
 *          is typically used for error messages. After printing the message,
 *          the program is terminated using `exit(EXIT_FAILURE)`, which ensures
 *          that the process stops and returns a failure status to the operating
 *          system.
 *
 * @return void
 */
void panic(const string format, ...) {
  va_list args;

  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);

  exit(EXIT_FAILURE);
}

/**
 * @brief TODO function.
 *
 * This function serves as a placeholder to signal parts of the code that
 * have not yet been implemented. When invoked, it prints a formatted message
 * to `stderr` indicating that the feature or functionality is "to be done",
 * and then terminates the program using `exit(EXIT_FAILURE)`.
 *
 * @param format A string that describes what is "TODO". This string can
 *        include format specifiers for additional details about the unimplemented
 *        functionality.
 *
 * @param ... The values that correspond to the format specifiers in the
 *        `format` string. These values are passed as a variable argument list
 *        using the `...` notation.
 *
 * @details Like the `panic` function, `todo` also uses `va_list`, `va_start`,
 *          `va_end`, and `vfprintf` to handle and print a formatted message.
 *          It helps track unfinished or future work and immediately stops
 *          the execution to avoid undefined behavior from incomplete code
 *          paths.
 *
 * @return void
 */
void todo(const string format, ...) {
  va_list args;

  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);

  exit(EXIT_FAILURE);
}
