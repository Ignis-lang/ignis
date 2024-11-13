#ifndef __DEBUG
#include "./array.h"
#include "../errors/errors.h"
#include "../memory/memory.h"
#else
#include <array/array.h>
#include <errors/errors.h>
#include <memory/memory.h>
#endif

IgnisArray *arrayNew(IgnisType type, u32 length, void *data) {
  IgnisArray *array = (IgnisArray *) allocate(sizeof(IgnisArray));

  if (!array) {
    panic("Failed to allocate memory for array");
  }

  array->length = length;
  array->data = data;
  array->elementSize = TYPE_SIZES[type];
  array->type = type;

  return array;
}

void freeArray(IgnisArray *array) {
  if (array->data) {
    freeMemory(array->data);
  }

  freeMemory(array);
}

u32 arrayLength(IgnisArray *array) { return array->length; }
