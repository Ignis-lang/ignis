#ifndef __DEBUG
#include "./char.h"
#else
#include <char/char.h>
#endif

#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char charToUpper(char value) {
  char buffer = (char) toupper(value);
  return buffer;
}

char charToLower(char value) {
  char buffer = (char) tolower(value);
  return buffer;
}

boolean charIsDigit(char value) {
  boolean result = isdigit(value);
  return result;
}

boolean charIsLetter(char value) {
  boolean result = isalpha(value);
  return result;
}

boolean charIsWhitespace(char value) {
  boolean result = isspace(value);
  return result;
}

unknown charToNumber(char value_, IgnisType type) {
  if (value_ == ' ') {
    return NULL;
  }

  IgnisGenericType *value = ignisGenericTypeNew(type);

  switch (type) {
  case TYPE_I8:
    value->data.i8 = (value->data.i8);
    break;
  case TYPE_I16:
    value->data.i16 = abs(value->data.i16);
    break;
  case TYPE_I32:
    value->data.i32 = abs(value->data.i32);
    break;
  case TYPE_I64:
    value->data.i64 = labs(value->data.i64);
    break;
  case TYPE_U8:
    value->data.u8 = (value->data.u8);
    break;
  case TYPE_U16:
    value->data.u16 = (value->data.u16);
    break;
  case TYPE_U32:
    value->data.u32 = (value->data.u32);
    break;
  case TYPE_U64:
    value->data.u64 = (value->data.u64);
    break;
  case TYPE_F32:
    value->data.f32 = fabsf(value->data.f32);
    break;
  case TYPE_F64:
    value->data.f64 = fabs(value->data.f64);
    break;
  default:
    ignisGenericTypeFree(value);
    return NULL;
    break;
  }

  return value;
}

u8 charCharCode(char value) {
  u8 buffer = (u8) value;
  return buffer;
}
