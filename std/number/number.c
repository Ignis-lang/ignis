#ifndef __DEBUG
#include "./number.h"
#include "../memory/memory.h"
#else
#include <memory/memory.h>
#include <number/number.h>
#endif

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

string u8ToString(u8 value) {
  string buffer = (string) allocate(4);
  sprintf(buffer, "%u", value);
  return buffer;
}

string u16ToString(u16 value) {
  string buffer = (string) allocate(6);
  sprintf(buffer, "%u", value);
  return buffer;
}

string u32ToString(u32 value) {
  string buffer = (string) allocate(11);
  sprintf(buffer, "%u", value);
  return buffer;
}

string u64ToString(u64 value) {
  string buffer = (string) allocate(21);
  sprintf(buffer, "%lu", value);
  return buffer;
}

string i8ToString(i8 value) {
  string buffer = (string) allocate(5);
  sprintf(buffer, "%d", value);
  return buffer;
}

string i16ToString(i16 value) {
  string buffer = (string) allocate(7);

  sprintf(buffer, "%d", value);

  return buffer;
}

string i32ToString(i32 value) {
  string buffer = (string) allocate(12);

  sprintf(buffer, "%d", value);

  return buffer;
}

string i64ToString(i64 value) {
  string buffer = (string) allocate(22);
  sprintf(buffer, "%ld", value);
  return buffer;
}

string f32ToString(f32 value) {
  string buffer = (string) allocate(16);
  sprintf(buffer, "%f", value);
  return buffer;
}

string f64ToString(f64 value) {
  string buffer = (string) allocate(32);

  sprintf(buffer, "%lf", value);

  return buffer;
}

string numberToString(unknown value, IgnisType type) {
  if (value == NULL) {
    return NULL;
  }

  switch (type) {
  case TYPE_I8:
    return u8ToString(value->data.i8);
    break;
  case TYPE_I16:
    return u16ToString(value->data.i16);
    break;
  case TYPE_I32:
    return u32ToString(value->data.i32);
    break;
  case TYPE_I64:
    return u64ToString(value->data.i64);
    break;
  case TYPE_U8:
    return u8ToString(value->data.u8);
    break;
  case TYPE_U16:
    return u16ToString(value->data.u16);
    break;
  case TYPE_U32:
    return u32ToString(value->data.u32);
    break;
  case TYPE_U64:
    return u64ToString(value->data.u64);
    break;
  case TYPE_F32:
    return f32ToString(value->data.f32);
    break;
  case TYPE_F64:
    return f64ToString(value->data.f64);
    break;
  default:
    break;
  }

  return NULL;
}

unknown numberAbs(unknown inputValue, IgnisType type) {
  if (inputValue == NULL) {
    return NULL;
  }

  IgnisGenericType *value = ignisGenericTypeNew(type);

  switch (type) {
  case TYPE_I8:
    value->data.i8 = (inputValue->data.i8);
    break;
  case TYPE_I16:
    value->data.i16 = abs(inputValue->data.i16);
    break;
  case TYPE_I32:
    value->data.i32 = abs(inputValue->data.i32);
    break;
  case TYPE_I64:
    value->data.i64 = labs(inputValue->data.i64);
    break;
  case TYPE_U8:
    value->data.u8 = (inputValue->data.u8);
    break;
  case TYPE_U16:
    value->data.u16 = (inputValue->data.u16);
    break;
  case TYPE_U32:
    value->data.u32 = (inputValue->data.u32);
    break;
  case TYPE_U64:
    value->data.u64 = (inputValue->data.u64);
    break;
  case TYPE_F32:
    value->data.f32 = fabsf(inputValue->data.f32);
    break;
  case TYPE_F64:
    value->data.f64 = fabs(inputValue->data.f64);
    break;
  default:
    ignisGenericTypeFree(value);
    return NULL;
    break;
  }

  return value;
}
