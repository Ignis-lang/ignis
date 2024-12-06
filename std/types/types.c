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
