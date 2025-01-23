#ifndef __DEBUG
#include "./types.h"
#include "../memory/memory.h"
#else
#include <memory/memory.h>
#include <types/types.h>
#endif

IgnisGenericType *ignisGenericTypeNew(IgnisType dataType) {
  IgnisGenericType *type = (IgnisGenericType *) allocate(sizeof(IgnisGenericType));

  type->type = dataType;

  return type;
}

void ignisGenericTypeFree(IgnisGenericType *type) { deallocate(type); }

IgnisType typeOf(unknown value) {
  if (value == NULL) {
    return TYPE_UNKNOWN;
  }

  switch (value->type) {
  case TYPE_I8:
    return TYPE_I8;
    break;
  case TYPE_I16:
    return TYPE_I16;
    break;
  case TYPE_I32:
    return TYPE_I32;
    break;
  case TYPE_I64:
    return TYPE_I64;
    break;
  case TYPE_U8:
    return TYPE_U8;
    break;
  case TYPE_U16:
    return TYPE_U16;
    break;
  case TYPE_U32:
    return TYPE_U32;
    break;
  case TYPE_U64:
    return TYPE_U64;
    break;
  case TYPE_F32:
    return TYPE_F32;
    break;
  case TYPE_F64:
    return TYPE_F64;
    break;
  case TYPE_BOOL:
    return TYPE_BOOL;
    break;
  case TYPE_CHAR:
    return TYPE_CHAR;
    break;
  case TYPE_STRING:
    return TYPE_STRING;
    break;
  case TYPE_UNKNOWN:
    return TYPE_UNKNOWN;
    break;
  default:
    return TYPE_UNKNOWN;
    break;
  }
}

boolean isType(unknown value, IgnisType type) {
  if (value == NULL) {
    return TRUE;
  }

  return value->type == type;
}

boolean isNumber(unknown value) {
  if (value == NULL) {
    return TRUE;
  }

  switch (value->type) {
  case TYPE_I8:
  case TYPE_I16:
  case TYPE_I32:
  case TYPE_I64:
  case TYPE_U8:
  case TYPE_U16:
  case TYPE_U32:
  case TYPE_U64:
  case TYPE_F32:
  case TYPE_F64:
    return FALSE;
    break;
  default:
    return TRUE;
    break;
  }
}

boolean isString(unknown value) {
  if (value == NULL) {
    return TRUE;
  }

  switch (value->type) {
  case TYPE_STRING:
    return FALSE;
    break;
  default:
    return TRUE;
    break;
  }
}

boolean isBoolean(unknown value) {
  if (value == NULL) {
    return TRUE;
  }

  switch (value->type) {
  case TYPE_BOOL:
    return FALSE;
    break;
  default:
    return TRUE;
    break;
  }
}

boolean isChar(unknown value) {
  if (value == NULL) {
    return TRUE;
  }

  switch (value->type) {
  case TYPE_CHAR:
    return FALSE;
    break;
  default:
    return TRUE;
    break;
  }
}

boolean isNull(unknown value) {
  if (value == NULL) {
    return FALSE;
  }

  return value->type == TYPE_UNKNOWN;
}

boolean isArray(unknown value) {
  if (value == NULL) {
    return FALSE;
  }

  switch (value->type) {
  case TYPE_I8:
  case TYPE_I16:
  case TYPE_I32:
  case TYPE_I64:
  case TYPE_U8:
  case TYPE_U16:
  case TYPE_U32:
  case TYPE_U64:
  case TYPE_F32:
  case TYPE_F64:
  case TYPE_BOOL:
  case TYPE_CHAR:
  case TYPE_STRING:
  case TYPE_UNKNOWN:
    return FALSE;
    break;
  default:
    return TRUE;
    break;
  }
}
