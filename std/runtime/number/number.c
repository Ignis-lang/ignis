#include "number.h"
#include <stdio.h>
#include <stdlib.h>

#define INT_BUF_SIZE 24
#define FLOAT_BUF_SIZE 32

string i8ToString(i8 value) {
  string buf = (string)malloc(INT_BUF_SIZE);
  if (buf == NULL)
    return NULL;
  snprintf(buf, INT_BUF_SIZE, "%d", (int)value);
  return buf;
}

string i16ToString(i16 value) {
  string buf = (string)malloc(INT_BUF_SIZE);
  if (buf == NULL)
    return NULL;
  snprintf(buf, INT_BUF_SIZE, "%d", (int)value);
  return buf;
}

string i32ToString(i32 value) {
  string buf = (string)malloc(INT_BUF_SIZE);
  if (buf == NULL)
    return NULL;
  snprintf(buf, INT_BUF_SIZE, "%d", value);
  return buf;
}

string i64ToString(i64 value) {
  string buf = (string)malloc(INT_BUF_SIZE);
  if (buf == NULL)
    return NULL;
  snprintf(buf, INT_BUF_SIZE, "%ld", value);
  return buf;
}

string u8ToString(u8 value) {
  string buf = (string)malloc(INT_BUF_SIZE);
  if (buf == NULL)
    return NULL;
  snprintf(buf, INT_BUF_SIZE, "%u", (unsigned int)value);
  return buf;
}

string u16ToString(u16 value) {
  string buf = (string)malloc(INT_BUF_SIZE);
  if (buf == NULL)
    return NULL;
  snprintf(buf, INT_BUF_SIZE, "%u", (unsigned int)value);
  return buf;
}

string u32ToString(u32 value) {
  string buf = (string)malloc(INT_BUF_SIZE);
  if (buf == NULL)
    return NULL;
  snprintf(buf, INT_BUF_SIZE, "%u", value);
  return buf;
}

string u64ToString(u64 value) {
  string buf = (string)malloc(INT_BUF_SIZE);
  if (buf == NULL)
    return NULL;
  snprintf(buf, INT_BUF_SIZE, "%lu", value);
  return buf;
}

string f32ToString(f32 value) {
  string buf = (string)malloc(FLOAT_BUF_SIZE);
  if (buf == NULL)
    return NULL;
  snprintf(buf, FLOAT_BUF_SIZE, "%g", (double)value);
  return buf;
}

string f64ToString(f64 value) {
  string buf = (string)malloc(FLOAT_BUF_SIZE);
  if (buf == NULL)
    return NULL;
  snprintf(buf, FLOAT_BUF_SIZE, "%g", value);
  return buf;
}

string booleanToString(boolean value) {
  if (value) {
    string buf = (string)malloc(5);
    if (buf == NULL)
      return NULL;
    buf[0] = 't';
    buf[1] = 'r';
    buf[2] = 'u';
    buf[3] = 'e';
    buf[4] = '\0';
    return buf;
  } else {
    string buf = (string)malloc(6);
    if (buf == NULL)
      return NULL;
    buf[0] = 'f';
    buf[1] = 'a';
    buf[2] = 'l';
    buf[3] = 's';
    buf[4] = 'e';
    buf[5] = '\0';
    return buf;
  }
}
