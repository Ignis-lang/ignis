#include "string.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

u64 stringLength(const string s) {
  if (s == NULL)
    return 0;
  return (u64)strlen(s);
}

i32 stringCompare(const string a, const string b) {
  if (a == NULL && b == NULL)
    return 0;
  if (a == NULL)
    return -1;
  if (b == NULL)
    return 1;
  return (i32)strcmp(a, b);
}

string stringConcat(const string a, const string b) {
  if (a == NULL && b == NULL)
    return NULL;
  if (a == NULL)
    return strdup(b);
  if (b == NULL)
    return strdup(a);

  size_t len_a = strlen(a);
  size_t len_b = strlen(b);
  string result = (string)malloc(len_a + len_b + 1);
  if (result == NULL)
    return NULL;

  memcpy(result, a, len_a);
  memcpy(result + len_a, b, len_b + 1);
  return result;
}

string stringSubString(const string s, i64 start, i64 len) {
  if (s == NULL || len <= 0)
    return NULL;

  size_t s_len = strlen(s);
  if (start < 0)
    start = 0;
  if ((size_t)start >= s_len)
    return strdup("");

  size_t actual_len = (size_t)len;
  if ((size_t)start + actual_len > s_len) {
    actual_len = s_len - (size_t)start;
  }

  string result = (string)malloc(actual_len + 1);
  if (result == NULL)
    return NULL;

  memcpy(result, s + start, actual_len);
  result[actual_len] = '\0';
  return result;
}

i8 stringCharAt(const string s, i64 index) {
  if (s == NULL || index < 0)
    return 0;
  size_t len = strlen(s);
  if ((size_t)index >= len)
    return 0;
  return (i8)s[index];
}

i64 stringIndexOf(const string haystack, const string needle) {
  if (haystack == NULL || needle == NULL)
    return -1;

  char *pos = strstr(haystack, needle);
  if (pos == NULL)
    return -1;
  return (i64)(pos - haystack);
}

boolean stringContains(const string haystack, const string needle) {
  return stringIndexOf(haystack, needle) >= 0 ? TRUE : FALSE;
}

string stringToUpperCase(const string s) {
  if (s == NULL)
    return NULL;

  size_t len = strlen(s);
  string result = (string)malloc(len + 1);
  if (result == NULL)
    return NULL;

  for (size_t i = 0; i < len; i++) {
    result[i] = (char)toupper((unsigned char)s[i]);
  }
  result[len] = '\0';
  return result;
}

string stringToLowerCase(const string s) {
  if (s == NULL)
    return NULL;

  size_t len = strlen(s);
  string result = (string)malloc(len + 1);
  if (result == NULL)
    return NULL;

  for (size_t i = 0; i < len; i++) {
    result[i] = (char)tolower((unsigned char)s[i]);
  }
  result[len] = '\0';
  return result;
}

void stringPushChar(string *s, i8 c) {
  if (s == NULL)
    return;

  if (*s == NULL) {
    *s = (string)malloc(2);
    if (*s == NULL)
      return;
    (*s)[0] = (char)c;
    (*s)[1] = '\0';
    return;
  }

  size_t len = strlen(*s);
  string new_s = (string)realloc(*s, len + 2);
  if (new_s == NULL)
    return;

  new_s[len] = (char)c;
  new_s[len + 1] = '\0';
  *s = new_s;
}
