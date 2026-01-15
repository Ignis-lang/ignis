#include "string.h"
#include <ctype.h>
#include <string.h>

// =============================================================================
// Length and basic operations
// =============================================================================

u64 stringLength(const string s) {
  if (s == NULL) {
    return 0;
  }
  return (u64)ignis_string_len(s);
}

i32 stringCompare(const string a, const string b) {
  const char *a_cstr = (a != NULL) ? ignis_string_cstr(a) : "";
  const char *b_cstr = (b != NULL) ? ignis_string_cstr(b) : "";

  return (i32)strcmp(a_cstr, b_cstr);
}

// =============================================================================
// String construction
// =============================================================================

string stringConcat(const string a, const string b) {
  string result = ignis_string_new();
  if (result == NULL) {
    return NULL;
  }

  if (a != NULL) {
    ignis_string_push_str(result, a);
  }
  if (b != NULL) {
    ignis_string_push_str(result, b);
  }

  return result;
}

string stringSubstring(const string s, i64 start, i64 len) {
  if (s == NULL || len <= 0) {
    return ignis_string_new();
  }

  size_t s_len = ignis_string_len(s);
  const char *s_data = ignis_string_cstr(s);

  // Clamp start
  if (start < 0) {
    start = 0;
  }
  if ((size_t)start >= s_len) {
    return ignis_string_new();
  }

  // Clamp length
  size_t actual_len = (size_t)len;
  if ((size_t)start + actual_len > s_len) {
    actual_len = s_len - (size_t)start;
  }

  return ignis_string_from_len(s_data + start, actual_len);
}

// =============================================================================
// Character access
// =============================================================================

char stringCharAt(const string s, i64 index) {
  if (s == NULL || index < 0) {
    return 0;
  }

  size_t len = ignis_string_len(s);
  if ((size_t)index >= len) {
    return 0;
  }

  return ignis_string_char_at(s, (size_t)index);
}

// =============================================================================
// Search functions
// =============================================================================

i64 stringIndexOf(const string haystack, const string needle) {
  if (haystack == NULL || needle == NULL) {
    return -1;
  }

  const char *h_cstr = ignis_string_cstr(haystack);
  const char *n_cstr = ignis_string_cstr(needle);

  const char *pos = strstr(h_cstr, n_cstr);
  if (pos == NULL) {
    return -1;
  }

  return (i64)(pos - h_cstr);
}

boolean stringContains(const string haystack, const string needle) {
  return stringIndexOf(haystack, needle) >= 0 ? TRUE : FALSE;
}

// =============================================================================
// Case conversion
// =============================================================================

string stringToUpperCase(const string s) {
  if (s == NULL) {
    return ignis_string_new();
  }

  size_t len = ignis_string_len(s);
  const char *data = ignis_string_cstr(s);

  string result = ignis_string_with_capacity(len + 1);
  if (result == NULL) {
    return NULL;
  }

  for (size_t i = 0; i < len; i++) {
    ignis_string_push_char(result, (char)toupper((unsigned char)data[i]));
  }

  return result;
}

string stringToLowerCase(const string s) {
  if (s == NULL) {
    return ignis_string_new();
  }

  size_t len = ignis_string_len(s);
  const char *data = ignis_string_cstr(s);

  string result = ignis_string_with_capacity(len + 1);
  if (result == NULL) {
    return NULL;
  }

  for (size_t i = 0; i < len; i++) {
    ignis_string_push_char(result, (char)tolower((unsigned char)data[i]));
  }

  return result;
}

// =============================================================================
// Mutation
// =============================================================================

void stringPushChar(string s, char c) {
  if (s == NULL) {
    return;
  }

  ignis_string_push_char(s, c);
}

// =============================================================================
// Empty string
// =============================================================================

string stringEmpty(void) { return ignis_string_new(); }
