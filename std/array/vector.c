#ifndef __DEBUG
#include "./vector.h"
#include "../errors/errors.h"
#include "../memory/memory.h"
#else
#include <vector/vector.h>
#include <errors/errors.h>
#include <memory/memory.h>
#endif

IgnisVector *vectorNew(IgnisType type, u32 length, void *data) {
  IgnisVector *vector = (IgnisVector *) allocate(sizeof(IgnisVector));

  if (!vector) {
    panic("Failed to allocate memory for vector");
  }

  vector->length = length;
  vector->data = data;
  vector->elementSize = TYPE_SIZES[type];
  vector->type = type;

  return vector;
}

void freeVector(IgnisVector *vector) {
  if (vector->data) {
    deallocate(vector->data);
  }

  deallocate(vector);
}

u32 vectorLength(IgnisVector *vector) { return vector->length; }
