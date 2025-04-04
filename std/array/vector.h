#ifndef IGNIS_STD_VECTOR_H
#define IGNIS_STD_VECTOR_H
#include <stdlib.h>
#ifndef __DEBUG
#include "../types/types.h"
#else
#include <types/types.h>
#endif

typedef struct IgnisVector {
  u32 length;
  u64 elementSize;
  IgnisType type;
  void *data;
} IgnisVector;

IgnisVector *vectorNew(IgnisType type, u32 length, void *data);
void freeVector(IgnisVector *vector);

// #region Methods with callback

u32 vector_length(IgnisVector *vector, string type);

typedef unknown (*IgnisMapFunction)(unknown value, u32 index, IgnisVector *vector);

IgnisVector *vectorMap(IgnisVector *vector, IgnisMapFunction fn);

typedef boolean (*IgnisFindFunction)(unknown value, u32 index, IgnisVector *vector);

IgnisVector *vectorFind(IgnisVector *vector, IgnisFindFunction fn);

typedef u32 (*IgnisFindIndexFunction)(unknown value, u32 index, IgnisVector *vector);

u32 vectorFindIndex(IgnisVector *vector, IgnisFindFunction fn);

typedef boolean (*IgnisFilterFunction)(unknown value, u32 index, IgnisVector *vector);

IgnisVector *vectorFilter(IgnisVector *vector, IgnisFilterFunction fn);

typedef unknown (*IgnisReduceFunction)(unknown value, u32 index, IgnisVector *vector);

IgnisVector *vectorReduce(IgnisVector *vector, IgnisReduceFunction fn, unknown vectorialValue);

typedef boolean (*IgnisSomeFunction)(unknown value, u32 index, IgnisVector *vector);

boolean vectorSome(IgnisVector *value, IgnisSomeFunction fn);

typedef boolean (*IgnisEveryFunction)(unknown value, u32 index, IgnisVector *vector);

boolean vectorEvery(IgnisVector *value, IgnisEveryFunction fn);

// #endregion

boolean vectorIncludes(IgnisVector *value, string search);

IgnisVector *vectorMax(IgnisVector *vector);

IgnisVector *vectorMin(IgnisVector *vector);

IgnisVector *vectorClone(IgnisVector *vector);

// #region Vector Mutation

void vectorPush(IgnisVector *vector, unknown value);

IgnisVector *vectorPop(unknown value, unknown vector);

IgnisVector *vectorShift(unknown value, unknown vector);

// #endregion

#endif
