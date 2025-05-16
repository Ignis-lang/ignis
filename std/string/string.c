/**
 * @file string.c
 * @brief The Ignis standard library string header.
 */
#ifdef __DEBUG
#include <errors/errors.h>
#include <memory/memory.h>
#include <string/strings.h>
#else
#include "../memory/memory.h"
#include "./string.h"
#endif

#include <memory.h>

#define __USE_C_STD

#ifdef __USE_C_STD
#include <ctype.h>
#include <stdio.h>
#endif

boolean stringIncludes(const string value, const string search) {
  return value == NULL ? 1 : strstr(value, search) != NULL;
}

string stringSplit(const string value, const string separator) {
  if (value == NULL || separator == NULL) {
    return NULL;
  }

  size_t length = strlen(value);
  string mutableValue = (string) allocate(length + 1);

  if (mutableValue == NULL) {
    return NULL;
  }

  strcpy(mutableValue, value);

  return strtok(mutableValue, separator);
}

char stringCharAt(const string value, int index) {
  if (value == NULL || index < 0 || index >= strlen(value)) {
    return '\0';
  }

  return value[index];
}

string stringToUpperCase(const string value) {
  if (value == NULL) {
    return NULL;
  }

  size_t length = strlen(value);
  string newValue = (string) allocate(length + 1);

  if (newValue == NULL) {
    return NULL;
  }

  for (int i = 0; i < length; i++) {
    newValue[i] = toupper(value[i]);
  }

  return newValue;
}

string stringToLowerCase(const string value) {
  if (value == NULL) {
    return NULL;
  }

  size_t length = strlen(value);
  string newValue = (string) allocate(length + 1);

  if (newValue == NULL) {
    return NULL;
  }

  for (int i = 0; i < length; i++) {
    newValue[i] = tolower(value[i]);
  }

  return newValue;
}

boolean stringToBoolean(const string value) { return value == NULL ? 1 : strcmp(value, "true") == 0; }

u8 *stringToBytes(const string value) {
  if (value == NULL) {
    return NULL;
  }

  u64 length = strlen(value);
  u8 *bytes = (u8 *) allocate(length * TYPE_SIZES[TYPE_U8]);

  if (bytes == NULL) {
    return NULL;
  }

  for (u64 i = 0; i < length; i++) {
    bytes[i] = value[i];
  }

  return bytes;
}

string stringTrim(const string value) {
  if (value == NULL) {
    return NULL;
  }

  u64 length = strlen(value);
  u64 start = 0;
  u64 end = length - 1;

  while (isspace(value[start])) {
    start++;
  }

  while (isspace(value[end])) {
    end--;
  }

  return stringSlice(value, start, end);
}

string stringTrimStart(const string value) {
  if (value == NULL) {
    return NULL;
  }

  u64 length = strlen(value);
  u64 start = 0;

  while (isspace(value[start])) {
    start++;
  }

  return stringSlice(value, start, length - 1);
}

string stringTrimEnd(const string value) {
  if (value == NULL) {
    return NULL;
  }

  u64 length = strlen(value);
  u64 end = length - 1;

  while (isspace(value[end])) {
    end--;
  }

  return stringSlice(value, 0, end);
}

boolean stringStartsWith(const string value, const string search) {
  if (value == NULL || search == NULL) {
    return 1;
  }

  u64 valueLength = strlen(value);
  u64 searchLength = strlen(search);

  if (searchLength > valueLength) {
    return 1;
  }

  for (u64 i = 0; i < searchLength; i++) {
    if (value[i] != search[i]) {
      return 1;
    }
  }

  return 0;
}

boolean stringEndWith(const string value, const string search) {
  if (value == NULL || search == NULL) {
    return 1;
  }

  u64 valueLength = strlen(value);
  u64 searchLength = strlen(search);

  if (searchLength > valueLength) {
    return 1;
  }

  for (u64 i = 0; i < searchLength; i++) {
    if (value[valueLength - searchLength + i] != search[i]) {
      return 1;
    }
  }

  return 0;
}

string stringSlice(const string value, int start, int end) {
  if (value == NULL || start < 0 || end < 0 || start > end || start >= strlen(value) || end >= strlen(value)) {
    return NULL;
  }

  u64 length = end - start + 1;
  string newValue = (string) allocate(length + 1);

  if (newValue == NULL) {
    return NULL;
  }

  for (u64 i = 0; i < length; i++) {
    newValue[i] = value[start + i];
  }

  return newValue;
}

string stringClone(const string value) {
  if (value == NULL) {
    return NULL;
  }

  u64 length = strlen(value);
  string newValue = (string) allocate(length + 1);

  if (newValue == NULL) {
    return NULL;
  }

  strcpy(newValue, value);

  return newValue;
}

int stringCount(const string value, const string search) {
  if (value == NULL || search == NULL) {
    return 0;
  }

  i32 count = 0;
  u64 valueLength = strlen(value);
  u64 searchLength = strlen(search);

  for (u64 i = 0; i < valueLength; i++) {
    if (stringStartsWith(value + i, search)) {
      count++;
      i += searchLength - 1;
    }
  }

  return count;
}

// #region Mutating Methods
void stringPush(string *value, string str) {
  if (*value == NULL || str == NULL) {
    return;
  }

  u64 valueLength = strlen(*value);
  u64 strLength = strlen(str);
  u64 newSize = valueLength + strLength + 1;
  string newValue = (string) allocate(newSize);

  if (newValue == NULL) {
    return;
  }

  strcpy(newValue, *value);
  strcat(newValue, str);
  deallocate(*value);

  *value = newValue;
}

char stringPop(string *value) {
  if (*value == NULL) {
    return '\0';
  }

  u64 length = strlen(*value);
  char last = (*value)[length - 1];

  (*value)[length - 1] = '\0';

  return last;
}

string stringConcat(string *value, string str) {
  if (*value == NULL || str == NULL) {
    return "";
  }

  u64 valueLength = strlen(*value);
  u64 strLength = strlen(str);
  u64 newSize = valueLength + strLength + 1;
  string newValue = (string) allocate(newSize);

  if (newValue == NULL) {
    return "";
  }

  strcpy(newValue, *value);
  strcat(newValue, str);

  *value = newValue;

  return newValue;
}

void stringClear(string *value) {
  if (*value == NULL) {
    return;
  }

  deallocate(*value);

  *value = NULL;
}

void stringReplace(string *value, const string search, const string replace) {
  if (*value == NULL || search == NULL || replace == NULL) {
    return;
  }

  u64 valueLength = strlen(*value);
  u64 searchLength = strlen(search);
  u64 replaceLength = strlen(replace);

  u64 newSize = valueLength + (replaceLength - searchLength) * stringCount(*value, search) + 1;
  string newValue = (string) allocate(newSize);

  if (newValue == NULL) {
    return;
  }

  string currentPos = *value;
  string newPos = newValue;

  while (*currentPos) {
    if (stringStartsWith(currentPos, search)) {
      strcpy(newPos, replace);
      newPos += replaceLength;
      currentPos += searchLength;
    } else {
      *newPos++ = *currentPos++;
    }
  }

  *newPos = '\0';

  deallocate(*value);
  *value = newValue;
}

// #endregion Mutating Methods
