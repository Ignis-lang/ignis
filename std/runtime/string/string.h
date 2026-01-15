#pragma once

#include "../ignis_rt.h"

/**
 * String helpers for std/string.
 *
 * All functions operate on `string` (alias of `IgnisString*`).
 */

/**
 * Returns the length of the string in bytes.
 *
 * @param s Input string.
 * @return Byte length (0 if s is NULL).
 */
u64 stringLength(const string s);
/**
 * Compares two strings lexicographically.
 *
 * @param a Left-hand string.
 * @param b Right-hand string.
 * @return Negative if a < b, zero if equal, positive if a > b.
 */
i32 stringCompare(const string a, const string b);

/**
 * Returns a new string with `a` followed by `b`.
 *
 * @param a Prefix string.
 * @param b Suffix string.
 * @return Newly allocated string (NULL on allocation failure).
 */
string stringConcat(const string a, const string b);
/**
 * Returns a substring from `start` with length `len`.
 *
 * @param s Source string.
 * @param start Start index (clamped to 0).
 * @param len Length in bytes.
 * @return Newly allocated substring (empty string on invalid range).
 */
string stringSubstring(const string s, i64 start, i64 len);

/**
 * Returns the character at `index`.
 *
 * @param s Source string.
 * @param index Zero-based index.
 * @return Character at index or 0 if out of range.
 */
char stringCharAt(const string s, i64 index);

/**
 * Returns the first index of `needle` in `haystack`.
 *
 * @param haystack String to search.
 * @param needle Substring to find.
 * @return Index of the match, or -1 if not found.
 */
i64 stringIndexOf(const string haystack, const string needle);
/**
 * Returns whether `needle` appears in `haystack`.
 *
 * @param haystack String to search.
 * @param needle Substring to find.
 * @return TRUE if found, FALSE otherwise.
 */
boolean stringContains(const string haystack, const string needle);

/**
 * Returns a new string with C `toupper` semantics applied.
 *
 * @param s Source string.
 * @return Newly allocated uppercase string.
 */
string stringToUpperCase(const string s);
/**
 * Returns a new string with C `tolower` semantics applied.
 *
 * @param s Source string.
 * @return Newly allocated lowercase string.
 */
string stringToLowerCase(const string s);

/**
 * Appends a character to the string in place.
 *
 * @param s Target string.
 * @param c Character to append.
 */
void stringPushChar(string s, char c);
