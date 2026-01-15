#pragma once

#include "../ignis_rt.h"

/**
 * I/O helpers for std/io.
 */

/**
 * Writes the string to stdout.
 *
 * @param s String to print.
 */
void print(string s);
/**
 * Writes the string to stderr.
 *
 * @param s String to print.
 */
void eprint(string s);
